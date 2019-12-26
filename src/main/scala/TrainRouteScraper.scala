package main

import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{allText, element, elementList}
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._

final case class Route(
                        station: String,
                        isBold: Boolean,
                        arrival: Option[String],
                        departure: Option[String],
                        platform: Option[Int],
                        connections: Option[String]
                      )

object TrainRouteScraper {
  def loadDocument(composition: Int, route: String): Browser#DocumentType = {
    val url = s"https://www.zelpage.cz/razeni/$composition/trasa-vlaku/$route"
    JsoupBrowser().get(url)
  }

  def getRoute(document: Browser#DocumentType): Seq[Route] = {
    val table = (document >> elementList(".ramecek_raz table")).lift(1)
    val rows = table match {
      case Some(table) => (table >> elementList("tr")).drop(1)
      case None => Seq()
    }

    rows.map(row => {
      val children = row.children.toSeq

      val station = children.head >> allText
      val isBold = (children.head >?> element("strong")).isDefined
      val arrival = Format.timeToISO(children(2) >> allText)
      val departure = Format.timeToISO(children(3) >> allText)
      val platform = (children(4) >> allText).toIntOption
      val connections = (children(5) >> allText).trim match {
        case "" => None
        case s => Some(s)
      }

      Route(station, isBold, arrival, departure, platform, connections)
    })
  }
}
