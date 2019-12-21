package main

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model._

import scala.collection.mutable.ListBuffer

case class Train(
                  Variant: Option[String],
                  Cars: Seq[String],
                  Updated: String,
                  UpdatedBy: String,
                  Notes: String,
                  Carrier: String,
                )

// https://www.zelpage.cz/razeni/20/vlaky/cd-4720

object Main extends App {
  def get_trains(route: String) = {
    val browser = JsoupBrowser()
    val doc = browser.get("https://www.zelpage.cz/razeni/20/vlaky/" ++ route)

    val rows = doc >> elementList(".ramecek_raz tr")

    var trainRowsList = new ListBuffer[ListBuffer[Element]]()
    trainRowsList += new ListBuffer[Element]()
    for (row <- rows) {
      if (trainRowsList.length > 0)
        trainRowsList.last.addOne(row)

      if ((row >> allText).startsWith("Dopravce vlaku:")) {
        trainRowsList += new ListBuffer[Element]()
      }
    }

    val trains = new ListBuffer[Train]()
    for (trainRows <- trainRowsList) {
      var variant: Option[String] = None
      var cars: Seq[String] = List()
      var updated = ""
      var updatedBy = ""
      var notes = ""
      var carrier = ""

      for (trainRow <- trainRows) {
        if ((trainRow >> allText).startsWith("Varianta:")) {
          variant = Some(trainRow >> allText)
        }

        trainRow >?> element(".obsah_raz") match {
          case None =>
          case Some(trainElement: Element) =>
            val trainImages = trainElement >> elementList("img").map(_ >> attr("src"))
            cars = trainImages.map(get_train_name_from_url)
        }

        if ((trainRow >> allText).startsWith("Aktualizace:")) {
          updated = trainRow >> allText
        }

        if ((trainRow >> allText).startsWith("Poznámky k vlaku:")) {
          notes = trainRow >> allText
        }

        if ((trainRow >> allText).startsWith("Dopravce vlaku:")) {
          carrier = trainRow >> allText
        }
      }

      trains.addOne(Train(variant, cars, updated, updatedBy, notes, carrier))
    }

    trains.take(trains.length - 1).toList foreach println
  }

  def get_train_name_from_url(url: String) = {
    url.split('/').takeRight(1)(0).split('.').take(1)(0)
  }


  println("cd-4720")
  get_trains("cd-4720")

  println("")
  println("cd-19900")
  get_trains("cd-19900")
}