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

object Main extends App {
  def get_trains(year: String, route: String) = {
    val url = s"https://www.zelpage.cz/razeni/$year/vlaky/$route"
    val document = JsoupBrowser().get(url)

    val rows = document >> elementList(".ramecek_raz tr")

    var groupedRows = new ListBuffer[ListBuffer[Element]]()
    groupedRows += new ListBuffer[Element]()
    for (row <- rows) {
      if (groupedRows.length > 0)
        groupedRows.last.addOne(row)

      if ((row >> allText).startsWith("Dopravce vlaku:")) {
        groupedRows += new ListBuffer[Element]()
      }
    }

    val trains = groupedRows.map(trainRows => {
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
          case Some(trainElement) =>
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

      Train(variant, cars, updated, updatedBy, notes, carrier)
    })

    trains.take(trains.length - 1).toSeq
  }

  def get_train_name_from_url(url: String) = {
    url.split('/').takeRight(1)(0).split('.').take(1)(0)
  }


  println("cd-4720")
  get_trains("20", "cd-4720") foreach println

  println("")
  println("cd-19900")
  get_trains("20", "cd-19900") foreach println
}