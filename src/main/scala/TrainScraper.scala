package main

import net.ruippeixotog.scalascraper.model.{Document, Element, ElementNode, TextNode}
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{allText, attr, element, elementList}
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._

final case class Carriage(
                           name: String,
                           livery: String,
                           vkm: String,
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
                               trains: Seq[Train],
                               route: Seq[Route]
                             )


object TrainScraper {
  def loadDocument(composition: Int, route: String): Document = {
    val url = s"https://www.zelpage.cz/razeni/$composition/vlaky/$route"
    JsoupBrowser().get(url)
  }

  def getTrainDetails(document: Document, route: Seq[Route] = Seq()): TrainDetails = {
    val title = (document >?> element(".titulek_raz")).flatMap(_.childNodes.toSeq.headOption.collect {
      case TextNode(text) => text.trim.filterNot("„“".toSet)
    })

    val groupedRows =
      (document >> elementList(".ramecek_raz tr"))
        .foldLeft(Seq(Seq[Element]()))((groups, row) => {
          val column = row >> element("td")

          if ((row >> allText).startsWith("Dopravce vlaku:"))
            groups.init :+ (groups.last :+ column) :+ Seq()
          else
            groups.init :+ (groups.last :+ column)
        })

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

        // Last Updated
        if (text.startsWith("Aktualizace:")) {
          updated = ("""\d+\.\d+\.\d+""".r findFirstIn text).flatMap(Format.dateToISO)
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


        // Carriages
        val trainElement = trainRow >?> element(".obsah_raz")
        if (trainElement.isDefined) {
          carriages = (trainElement.get >> elementList("img"))
            .filterNot(_.attr("src").contains("spacer"))
            .map(_ >> (attr("src"), attr("onmouseover")))
            .map {
              case (img, js) => (img, """(?<=')(.*)(?=')""".r findFirstIn js)
            }
            .map {
              case (img, Some(id)) => getCarriage(document, id, img)
              case (_, None) => Carriage("", "", "", None, None)
            }
        }
      }

      Train(variant, carriages, updated, updatedBy, notes, carrier, carrierURL)
    })

    // Remove last element from list because the row grouping doesn't know
    // how many trains there are and always creates an extra one.
    TrainDetails(title, trains.take(trains.length - 1), route)
  }

  def getCarriage(document: Document, id: String, img: String): Carriage = {
    val livery = img.split('/').takeRight(1).head.split('.').take(1).head
    val carriageElement = document >> element(s"#$id")

    val (name, vkm) = {
      val popupHead = carriageElement >> allText("h5")

      val pattern = """(.+) \[(.+)\]""".r
      val pattern(name, vkm) = popupHead

      (name, vkm)
    }

    val coachNo = {
      val spanText = carriageElement >> allText("span:first-of-type")
      val pattern = """(?<=\. )(.*)""".r
      (pattern findFirstIn spanText)
        .flatMap(_.toIntOption)
    }

    val route = carriageElement
      .childNodes
      .collect {
        case TextNode(text) => text
      }
      .map(_.trim)
      .find(_.nonEmpty)

    Carriage(name, livery, vkm, coachNo, route)
  }
}
