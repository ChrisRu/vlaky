package main

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._

object Main extends App {
  def get_trains(route: String) = {
    val browser = JsoupBrowser()
    val doc = browser.get("https://www.zelpage.cz/razeni/20/vlaky/cd-" ++ route)

    val trainImages = doc >> elementList(".obsah_raz").map(_ >> elementList("img").map(_ >> attr("src")))
    trainImages.map(_.map(get_train_name_from_url))
  }

  def get_train_name_from_url(url: String) = {
    url.split('/').takeRight(1)(0).split('.').take(1)(0)
  }


  get_trains("4720") foreach println
}