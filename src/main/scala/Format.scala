package main

import java.time.{LocalDate, LocalTime}
import java.time.format.DateTimeFormatter

object Format {
  private val dateFormatter = DateTimeFormatter.ofPattern("d.M.yyyy")

  def dateToISO(date: String): Option[String] = {
    try {
      Some(
        LocalDate
          .parse(date, dateFormatter)
          .format(DateTimeFormatter.ISO_DATE)
      )
    } catch {
      case _: Exception => None
    }
  }

  private val timeFormatter = DateTimeFormatter.ofPattern("H.mm.ss")

  def timeToISO(time: String): Option[String] = {
    try {
      Some(
        LocalTime
          .parse(time, timeFormatter)
          .format(DateTimeFormatter.ISO_TIME)
      )
    } catch {
      case _: Exception => None
    }
  }
}
