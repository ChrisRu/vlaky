package main

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.{Directives, PathMatchers}
import akka.http.scaladsl.server.Directives._
import akka.stream.{ActorMaterializer, Materializer}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn

final case class Train(
                        Variant: Option[String],
                        Cars: Seq[String],
                        Updated: String,
                        UpdatedBy: String,
                        Notes: String,
                        Carrier: String,
                      )

final case class Trains(trains: List[Train])

trait JsonProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val trainFormat: RootJsonFormat[Train] = jsonFormat6(Train)
  implicit val trainsFormat: RootJsonFormat[Trains] = jsonFormat1(Trains)
}

object Main extends Directives with JsonProtocol {
  def get_train_name_from_url(url: String): String =
    url.split('/').takeRight(1).head.split('.').take(1).head

  def get_trains(year: Int, route: String): Trains = {
    val url = s"https://www.zelpage.cz/razeni/$year/vlaky/$route"
    val document = JsoupBrowser().get(url)

    val rows = document >> elementList(".ramecek_raz tr")

    var groupedRows = new ListBuffer[ListBuffer[Element]]()
    groupedRows += new ListBuffer[Element]()
    for (row <- rows) {
      if (groupedRows.nonEmpty)
        groupedRows.last.addOne(row)

      if ((row >> allText).startsWith("Dopravce vlaku:"))
        groupedRows += new ListBuffer[Element]()
    }

    val trains = groupedRows.map(f = trainRows => {
      var variant: Option[String] = None
      var cars: Seq[String] = List()
      var updated = ""
      val updatedBy = ""
      var notes = ""
      var carrier = ""

      for (trainRow <- trainRows) {
        if ((trainRow >> allText).startsWith("Varianta:"))
          variant = Some(trainRow.children.map(x => x >> allText).mkString(" "))

        trainRow >?> element(".obsah_raz") match {
          case None =>
          case Some(trainElement) =>
            val trainImages = trainElement >> elementList("img").map(_ >> attr("src"))
            cars = trainImages.map(get_train_name_from_url)
        }

        if ((trainRow >> allText).startsWith("Aktualizace:"))
          updated = trainRow >> allText

        if ((trainRow >> allText).startsWith("Poznámky k vlaku:"))
          notes = trainRow >> allText

        if ((trainRow >> allText).startsWith("Dopravce vlaku:"))
          carrier = trainRow >> allText
      }

      Train(variant, cars, updated, updatedBy, notes, carrier)
    })

    Trains(trains.take(trains.length - 1).toList)
  }


  def main(args: Array[String]) {
    implicit val system: ActorSystem = ActorSystem()
    implicit val executor: ExecutionContextExecutor = system.dispatcher
    implicit val materializer: Materializer = ActorMaterializer()

    val route =
      pathPrefix("razeni") {
        path(IntNumber) {
          year => {
            pathPrefix("vlaky") {
              path(PathMatchers.Segments) {
                route => {
                  println("test")
                  complete(get_trains(year, route(0)))
                }
              }
            }
          }
        }
      }

    val interface = "localhost"
    val port = 8080
    val bindingFuture = Http().bindAndHandle(route, interface, port)

    println(s"Server online at http://$interface:$port/\nPress RETURN to stop...")
    StdIn.readLine()

    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }

  //  println("cd-4720")
  //  get_trains("20", "cd-4720") foreach println

  //  println("")
  //  println("cd-19900")
  //  get_trains("20", "cd-19900") foreach println
}