package cnx

import scala.util.matching.Regex

case class GoogleFinance(marketOpenMinute: Int,
                         marketCloseMinute: Int,
                         interval: Int,
                         timezoneOffset: Int,
                         prices: List[GoogleFinance.Price])

object GoogleFinance {

  case class Price(time: Long,
                   close: BigDecimal,
                   high: BigDecimal,
                   low: BigDecimal,
                   open: BigDecimal,
                   volume: BigDecimal)

  val firstLine: Regex = "^EXCHANGE.*".r
  val OpenMinute: Regex = "^MARKET_OPEN_MINUTE=(\\d*)".r
  val CloseMinute: Regex = "^MARKET_CLOSE_MINUTE=(\\d*)".r
  val Interval: Regex = "^INTERVAL=(\\d*)".r
  val TimeZoneOffset: Regex = "^TIMEZONE_OFFSET=(\\d*)".r
  val PriceDate: Regex = "a(\\d*)".r

  def getPrices(days: Int, quote: String, client: String => String): GoogleFinance = {
    (client(s"https://finance.google.com/finance/getprices?p=${days}d&i=60&f=d,o,h,l,c,v&q=$quote")
      .split("[\\r?\\n]+")
      .map(_.trim)
      .filter(!_.isEmpty)
    match {
      case Array(
      firstLine(),
      OpenMinute(openMinute),
      CloseMinute(closeMinute),
      Interval(interval),
      _, _, _,
      TimeZoneOffset(timezoneOffset),
      rows@_*
      ) => (openMinute, closeMinute, interval, timezoneOffset, rows)
      case Array(
      firstLine(),
      OpenMinute(openMinute),
      CloseMinute(closeMinute),
      Interval(interval),
      _, _,
      TimeZoneOffset(timezoneOffset),
      rows@_*
      ) => (openMinute, closeMinute, interval, timezoneOffset, rows)
    }) match {
      case (openMinute, closeMinute, interval, timezoneOffset, rows) =>
        GoogleFinance(
          openMinute.toInt,
          closeMinute.toInt,
          interval.toInt,
          timezoneOffset.toInt,
          rows
            .map(_.split(","))
            .foldLeft((0L, List.empty[Price])) {
              case ((_, acc), Array(PriceDate(date), close, high, low, open, volume)) =>
                (date.toLong,
                  acc.:+(
                    Price(
                      date.toLong, //+ timezoneOffset.toInt * 60,
                      BigDecimal(close), BigDecimal(high), BigDecimal(low), BigDecimal(open), BigDecimal(volume))))
              case ((base, acc), Array(date, close, high, low, open, volume)) =>
                (base,
                  acc.:+(
                    Price(
                      base.toLong + date.toInt * interval.toInt, //+ timezoneOffset.toInt * 60,
                      BigDecimal(close), BigDecimal(high), BigDecimal(low), BigDecimal(open), BigDecimal(volume))))
            }._2)
    }
  }
}





