package main

import net.ruippeixotog.scalascraper.model.{Element, ElementNode, TextNode}
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{allText, attr, element, elementList}
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
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
                        carriages: Seq[Carriage],
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
  def dateToISO(date: String): String =
    LocalDate.parse(date, DateTimeFormatter.ofPattern("d.M.yyyy")).format(DateTimeFormatter.ISO_DATE)

  def getCarriage(document: Browser#DocumentType, popupId: String): Carriage = {
    val carriageId = """(?<=')(.*)(?=')""".r findFirstIn popupId
    val carriageElement = carriageId.map(id => document >> element(s"#$id"))

    val (name, vkm) = carriageElement match {
      case Some(e) =>
        val popupHead = e >> allText("h5")

        val pattern = """(.+) \[(.+)\]""".r
        val pattern(name, vkm) = popupHead

        (Some(name), Some(vkm))
      case None => (None, None)
    }

    val coachNo = carriageElement.flatMap(e =>
      ("""(?<=\. )(.*)""".r findFirstIn (e >> allText("span:first-of-type"))
        )
        .map(_.toInt)
    )

    val route = carriageElement.flatMap(
      _.childNodes.collect {
        case TextNode(text) => text
      }
        .map(_.trim)
        .find(_.nonEmpty)
    )

    Carriage(name, vkm, coachNo, route)
  }

  def getTrains(composition: Int, route: String): TrainDetails = {
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
      var carriages: Seq[Carriage] = Seq()
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
        val trainElement = trainRow >?> element(".obsah_raz")
        if (trainElement.isDefined) {
          carriages = (trainElement.get >> elementList("img"))
            .filterNot(_.attr("src").contains("spacer"))
            .map(e => getCarriage(document, e >> attr("onmouseover")))
        }

        // Last Updated
        if (text.startsWith("Aktualizace:")) {
          updated = ("""\d+\.\d+\.\d+""".r findFirstIn text).map(dateToISO)
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
          carrier = Some(trainRow >> allText("a"))
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
