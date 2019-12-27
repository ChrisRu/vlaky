package main

import net.ruippeixotog.scalascraper.model.{Document, Element, ElementNode, TextNode, Node}
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{allText, attr, element, elementList}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._

final case class Carriage(
                           name: Option[String],
                           livery: Option[String],
                           vkm: Option[String],
                           coachNo: Option[Int],
                           route: Option[String]
                         )

final case class Train(
                        variant: Option[String],
                        carriages: Seq[Carriage],
                        updated: Option[String],
                        updatedBy: Option[String],
                        notes: Option[Seq[String]],
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
    val title = (document >?> element(".titulek_raz"))
      .flatMap(_.childNodes.headOption.collect {
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
      var notes: Option[Seq[String]] = None
      var carrier: Option[String] = None
      var carrierURL: Option[String] = None

      for (trainRow <- trainRows) {
        val text = trainRow >> allText

        // Variant
        if (text.startsWith("Varianta:")) {
          variant = Some(
            trainRow
              .childNodes
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
            getNotes(trainRow.childNodes)
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
            .map(e => {
              val img = e >> attr("src")
              val js = e >> attr("onmouseover")
              val id = ("""(?<=')(.*)(?=')""".r findFirstIn js).getOrElse("")
              getCarriage(document, id, img)
            })
        }
      }

      Train(variant, carriages, updated, updatedBy, notes, carrier, carrierURL)
    })
      // Remove last element from list because the row grouping doesn't know
      // how many trains there are and always creates an extra one.
      .init

    TrainDetails(title, trains, route)
  }

  def parseLeadingIcon(note: Seq[Node]): String = {
    note
      .collect {
        case TextNode(text) => text
      }
      .map(_.trim)
      .map(_.replaceAll("""^-\s+""", ""))
      .filter(_.nonEmpty)
      .mkString("")
  }

  def parseRegularLine(note: Seq[Node]): String = {
    note
      .map {
        case TextNode(text) => text.trim
        case ElementNode(element) => element.tagName match {
          case "a" => (element >> allText).trim
          case "img" => " " + (element >> attr("alt")) + " "
        }
      }
      .filter(_.nonEmpty)
      .mkString("")
  }

  def getNotes(children: Iterable[Node]): Seq[String] = {
    children
      .foldLeft(Seq(Seq[Node]()))((groups, row) =>
        row match {
          case ElementNode(element) if element.tagName == "br" =>
            groups.init :+ groups.last :+ Seq()
          case _ =>
            groups.init :+ (groups.last :+ row)
        }
      )
      // Remove notes title 'Poznámky k vlaku:'
      .tail
      // Remove last group as the notes always end with a <br> tag
      .init
      .map(n => {
        // Remove empty TextNode at front of line
        val stripped = n.head match {
          case TextNode(text) if text.trim == "" => n.tail
          case _ => n
        }

        stripped.head match {
          case ElementNode(element) if element.tagName == "img" =>
            parseLeadingIcon(stripped)
          case _ =>
            parseRegularLine(stripped)
        }
      })
  }

  def getCarriage(document: Document, id: String, img: String): Carriage = {
    val carriageElement = document >?> element(s"#$id")

    val (name, vkm) = {
      val popupHead = (carriageElement >?> allText("h5")).flatten

      val pattern = """(.+) \[(.+)\]""".r

      popupHead.map(text =>
        pattern.findAllIn(text).subgroups
      ) match {
        case Some(x) => (x.headOption, x.lift(1))
        case None => (None, None)
      }
    }

    val livery = img.split('/').lastOption.flatMap(_.split('.').headOption)

    val coachNo = {
      val spanText = (carriageElement >?> allText("span:first-of-type")).flatten
      val pattern = """(?<=\. )(.*)""".r
      spanText.flatMap(text =>
        (pattern findFirstIn text)
          .flatMap(_.toIntOption)
      )
    }

    val route = carriageElement
      .flatMap(
        _
          .childNodes
          .collect {
            case TextNode(text) => text
          }
          .map(_.trim)
          .find(_.nonEmpty)
      )

    Carriage(name, livery, vkm, coachNo, route)
  }
}
