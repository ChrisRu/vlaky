package main

import java.time.LocalTime

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.Directives
import akka.stream.{ActorMaterializer, Materializer}
import spray.json.{DefaultJsonProtocol, RootJsonFormat}
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.io.StdIn

trait JsonProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val carriageFormat: RootJsonFormat[Carriage] = jsonFormat5(Carriage)
  implicit val routeFormat: RootJsonFormat[Route] = jsonFormat6(Route)
  implicit val trainFormat: RootJsonFormat[Train] = jsonFormat7(Train)
  implicit val trainsFormat: RootJsonFormat[TrainDetails] = jsonFormat3(TrainDetails)
}

object Main extends Directives with JsonProtocol {
  def main(args: Array[String]) {
    implicit val system: ActorSystem = ActorSystem()
    implicit val executor: ExecutionContextExecutor = system.dispatcher
    implicit val materializer: Materializer = ActorMaterializer()

    val route =
      cors() {
        get {
          pathPrefix("composition" / Segment / "train" / Segment) {
            (year, route) => {
              val composition = Integer.parseInt(year.length match {
                case 4 => if (year.startsWith("20")) year.slice(2, 4) else year
                case _ => year
              })

              val date = LocalTime.now()
              println(s"$date — Requesting train $route (20$composition timetable)")

              val trainRouteDocumentFuture = Future {
                TrainRouteScraper.loadDocument(composition, route)
              }
              val trainDocumentFuture = Future {
                TrainScraper.loadDocument(composition, route)
              }

              val trainDetails = for {
                trainDocument <- trainDocumentFuture
                trainRouteDocument <- trainRouteDocumentFuture

                trainRoute = TrainRouteScraper.getRoute(trainRouteDocument)
                trainDetails = TrainScraper.getTrainDetails(trainDocument, trainRoute)
              } yield trainDetails

              onSuccess(trainDetails) {
                trainDetails => complete(trainDetails)
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
}