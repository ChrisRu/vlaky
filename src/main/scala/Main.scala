package main

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.DateTime
import akka.http.scaladsl.server.{Directives, PathMatchers}
import akka.http.scaladsl.server.Directives._
import akka.stream.{ActorMaterializer, Materializer}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn

final case class Train(
                        variant: Option[String],
                        cars: Seq[String],
                        updated: Option[String],
                        updatedBy: Option[String],
                        notes: Option[String],
                        carrier: Option[String],
                        carrierURL: Option[String]
                      )

final case class Trains(title: Option[String], track: Option[String], trains: List[Train])

trait JsonProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val trainFormat: RootJsonFormat[Train] = jsonFormat7(Train)
  implicit val trainsFormat: RootJsonFormat[Trains] = jsonFormat3(Trains)
}

object Main extends Directives with JsonProtocol {
  def date_to_iso(date: String): String =
    LocalDate.parse(date, DateTimeFormatter.ofPattern("d.M.yyyy")).format(DateTimeFormatter.ISO_DATE)

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
        groupedRows.last.addOne(row >> element("td"))

      if ((row >> allText).startsWith("Dopravce vlaku:"))
        groupedRows += new ListBuffer[Element]()
    }

    val title = (document >> element(".titulek_raz")).childNodes.toSeq.head match {
      case TextNode(x) => Some(x.trim)
      case _ => None
    }

    var track: Option[String] = None

    val trains = groupedRows.map(f = trainRows => {
      var variant: Option[String] = None
      var cars: Seq[String] = List()
      var updated: Option[String] = None
      var updatedBy: Option[String] = None
      var notes: Option[String] = None
      var carrier: Option[String] = None
      var carrierURL: Option[String] = None

      for (trainRow <- trainRows) {
        if ((trainRow >> allText).startsWith("Trasa:")) {
          track = Some(
            trainRow
              .childNodes
              .toSeq
              .collect {
                case TextNode(x) => x.trim
              }
              .mkString("")
          )
        }

        if ((trainRow >> allText).startsWith("Varianta:")) {
          variant = Some(
            trainRow
              .childNodes
              .toSeq
              .collect {
                case TextNode(x) => x
                case ElementNode(x) => x.tagName match {
                  case "img" => x >> attr("alt")
                  case _ => ""
                }
              }
              .map(_.trim)
              .filter(_.nonEmpty)
              .mkString("")
          )
        }

        trainRow >?> element(".obsah_raz") match {
          case None =>
          case Some(trainElement) =>
            val trainImages = trainElement >> elementList("img").map(_ >> attr("src"))
            cars = trainImages.map(get_train_name_from_url)
        }

        if ((trainRow >> allText).startsWith("Aktualizace:")) {
          val content = trainRow >> allText

          updated = ("""\d+\.\d+\.\d+""".r findFirstIn content).map(date_to_iso)
          updatedBy = ("""\((.+)\)""".r findFirstMatchIn content).map(_.group(1))
        }

        if ((trainRow >> allText).startsWith("Poznámky k vlaku:")) {
          notes = Some(
            trainRow
              .childNodes
              .toSeq
              .collect {
                case TextNode(x) => x
              }
              .map(_.trim)
              .filter(_.nonEmpty)
              .mkString("\n")
          )
        }

        if ((trainRow >> allText).startsWith("Dopravce vlaku:")) {
          carrier = Some(trainRow >> element("a") >> allText)
          carrierURL = Some(trainRow >> element("a") >> attr("href"))
        }
      }

      Train(variant, cars, updated, updatedBy, notes, carrier, carrierURL)
    })

    Trains(title, track, trains.take(trains.length - 1).toList)
  }


  def main(args: Array[String]) {
    implicit val system: ActorSystem = ActorSystem()
    implicit val executor: ExecutionContextExecutor = system.dispatcher
    implicit val materializer: Materializer = ActorMaterializer()

    val route =
      pathPrefix("razeni") {
        pathPrefix("vlaky" / Segment) {
          route => {
            complete(get_trains(20, route))
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