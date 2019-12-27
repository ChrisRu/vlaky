package main

import main.TrainScraper.{parseLeadingIcon, parseRegularLine}
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{allText, attr, element, elementList}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.{Document, ElementNode, Node, TextNode}

final case class Departure(
                            arrival: Option[String],
                            departure: Option[String],
                            line: Option[String],
                            train: Option[String],
                            fromStation: Option[String],
                            toStation: Option[String],
                            platform: Option[String],
                            comment: Option[Seq[String]],
                          )

object TrainDepartures {
  def loadDocument(station: String): Document = {
    val url = s"https://www.zelpage.cz/odjezdy-2020/$station.html"
    JsoupBrowser().get(url)
  }

  def getDepartures(document: Document): Seq[Departure] = {
    val rows = document >> elementList(".ramecek_raz tr[onmouseover]")

    rows.map(row => {
      val children = row.children.toSeq

      val arrival = (children.headOption >> allText).flatMap(Format.timeToISO)
      val departure = (children.lift(1) >> allText).flatMap(Format.timeToISO)
      val line = (children.lift(2) >> allText).map(_.trim)
      val train = (children.lift(3) >> allText).map(_.trim)
      val fromStation = (children.lift(4) >> allText).map(_.trim)
      val toStation = (children.lift(5) >> allText).map(_.trim)
      val platform = (children.lift(6) >> allText).map(_.trim)
      val notes = children.lift(7).map(
        _.childNodes
          .foldLeft(Seq(Seq[Node]()))((groups, row) =>
            row match {
              case ElementNode(element) if element.tagName == "br" =>
                groups.init :+ groups.last :+ Seq()
              case _ =>
                groups.init :+ (groups.last :+ row)
            }
          )
          .map(n => {
            // Remove empty TextNode at front of line
            (n.head match {
              case TextNode(text) if text.trim == "" => n.tail
              case _ => n
            })
              .map {
                case TextNode(text) => text.trim
                case ElementNode(element) => element.tagName match {
                  case "img" => " " + (element >> attr("alt")) + " "
                  case _ => (element >> allText).trim
                }
              }
              .filter(_.nonEmpty)
              .mkString("")
          }))

      Departure(arrival, departure, line, train, fromStation, toStation, platform, notes)
    })
  }
}
