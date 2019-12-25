package main

import net.ruippeixotog.scalascraper.model.{Element, ElementNode, TextNode}
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{attr, element, elementList, allText}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._

import java.time.LocalDate
import java.time.format.DateTimeFormatter

final case class Carriage(
                           name: Option[String],
                           vkm: Option[String],
                           coachNo: Option[Int],
                           route: Option[String]
)

final case class Train(
                        variant: Option[String],
                        carriages: List[Carriage],
                        updated: Option[String],
                        updatedBy: Option[String],
                        notes: Option[String],
                        carrier: Option[String],
                        carrierURL: Option[String]
                      )

final case class TrainDetails(
                               title: Option[String],
                               track: Option[String],
                               trains: List[Train]
                             )


object TrainScraper {
  def get_trains(composition: Int, route: String): TrainDetails = {
    val url = s"https://www.zelpage.cz/razeni/$composition/vlaky/$route"
    val document = JsoupBrowser().get(url)

    val groupedRows =
      (document >> elementList(".ramecek_raz tr"))
        .foldLeft(List(List[Element]()))((groups, row) => {
          val column = row >> element("td")

          if ((row >> allText).startsWith("Dopravce vlaku:"))
            groups.init :+ (groups.last :+ column) :+ List()
          else
            groups.init :+ (groups.last :+ column)
        })

    val title = (document >?> element(".titulek_raz")).flatMap(_.childNodes.toSeq.headOption.collect {
      case TextNode(text) => text.trim.filterNot("„“".toSet)
    })

    var track: Option[String] = None

    val trains = groupedRows.map(trainRows => {
      var carriages: List[Carriage] = List()
      var variant: Option[String] = None
      var updated: Option[String] = None
      var updatedBy: Option[String] = None
      var notes: Option[String] = None
      var carrier: Option[String] = None
      var carrierURL: Option[String] = None

      for (trainRow <- trainRows) {
        val text = trainRow >> allText
        val childNodes = trainRow.childNodes.toSeq

        // Track
        if (text.startsWith("Trasa:")) {
          track = Some(
            childNodes
              .collect {
                case TextNode(text) => text.trim
              }
              .mkString("")
          )
        }

        // Variant
        if (text.startsWith("Varianta:")) {
          variant = Some(
            childNodes
              .collect {
                case TextNode(text) => text
                case ElementNode(element) => element.tagName match {
                  case "img" => element >> attr("alt")
                  case _ => ""
                }
              }
              .map(_.trim)
              .filter(_.nonEmpty)
              .mkString(" ")
          )
        }

        // Carriages
        val getPopupRef = (call: String) => """(?<=')(.*)(?=')""".r findFirstIn call
        val constructCarriage = (url: String, popupId: String) => {
            val ref = getPopupRef(popupId)

            val (name, vkm) = ref match {
              case Some(p) => {
                val popupHead = (document >> allText(s"#$p > h5"))

                val name = """(.*)(?= \[)""".r findFirstIn popupHead
                val vkm = """(?<=\[)(.*)(?=\])""".r findFirstIn popupHead

                (name, vkm)
              }
              case _ => (None, None)
            }

            val coachNo = ref match {
              case Some(p) => ("""(?<=\. )(.*)""".r findFirstIn (document >> allText(s"#$p > span:first-of-type"))).map(_.toInt)
              case _ => None
            }

            val route = ref match {
              case Some(p) => {
                val routeWrapped = (document >> element(s"#$p")).childNodes.collect {
                    case TextNode(text) => text
                  }
                  .toSeq
                  .map(_.trim)
                  .filter(_.nonEmpty)

                if (routeWrapped.nonEmpty) {
                  Some(routeWrapped.head)
                } else {
                  None
                }
              }
              case _ => None
            }

            Carriage(name, vkm, coachNo, route)
        }

        val trainElement = trainRow >?> element(".obsah_raz") 
        if (trainElement.isDefined) {
          val trainDetails = trainElement.get >> elementList("img").map(_ >> (attr("src"), attr("onmouseover")))

          carriages = trainDetails
            .filterNot(_._1.contains("spacer"))
            .map(d => constructCarriage(d._1, d._2))
        }

        // Last Updated
        if (text.startsWith("Aktualizace:")) {
          val date_to_iso = (date: String) =>
            LocalDate.parse(date, DateTimeFormatter.ofPattern("d.M.yyyy")).format(DateTimeFormatter.ISO_DATE)

          updated = ("""\d+\.\d+\.\d+""".r findFirstIn text).map(date_to_iso)
          updatedBy = ("""\((.+)\)""".r findFirstMatchIn text).map(_.group(1))
        }

        // Notes
        if (text.startsWith("Poznámky k vlaku:")) {
          notes = Some(
            childNodes
              .collect {
                case TextNode(text) => text
              }
              .map(_.trim)
              .filter(_.nonEmpty)
              .mkString("\n")
          )
        }

        // Carrier
        if (text.startsWith("Dopravce vlaku:")) {
          carrier = Some(trainRow >> element("a") >> allText)
          carrierURL = Some(trainRow >> element("a") >> attr("href"))
        }
      }

      Train(variant, carriages, updated, updatedBy, notes, carrier, carrierURL)
    })

    // Remove last element from list because the row grouping doesn't know
    // how many trains there are and always creates an extra one.
    TrainDetails(title, track, trains.take(trains.length - 1))
  }
}
