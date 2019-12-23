package main

import net.ruippeixotog.scalascraper.model.{Element, ElementNode, TextNode}
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{attr, element, elementList}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._

import java.time.LocalDate
import java.time.format.DateTimeFormatter

final case class Train(
                        variant: Option[String],
                        carriages: Seq[String],
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
            groups :+ List(column)
          else
            groups.init :+ (groups.last :+ column)
        })

    val title = (document >> element(".titulek_raz")).childNodes.toSeq.head match {
      case TextNode(text) => Some(text.trim)
      case _ => None
    }

    var track: Option[String] = None

    val trains = groupedRows.map(trainRows => {
      var cars: Seq[String] = List()
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
        val getTrainName = (url: String) => url.split('/').takeRight(1).head.split('.').take(1).head
        val trainElement = trainRow >?> element(".obsah_raz")
        if (trainElement.isDefined) {
          val trainUrls = trainElement.get >> elementList("img").map(_ >> attr("src"))
          cars = trainUrls.map(getTrainName)
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

      Train(variant, cars, updated, updatedBy, notes, carrier, carrierURL)
    })

    // Remove last element from list because the row grouping doesn't know
    // how many trains there are and always creates an extra one.
    TrainDetails(title, track, trains.take(trains.length - 1).toList)
  }
}
