package cnx

import java.net.Proxy

import scalaj.http.Http

object Test {
  def main(args: Array[String]): Unit = {
    val client: String => String =
      (url: String) => Http(url)
        .proxy("127.0.0.1", 1080, Proxy.Type.SOCKS) //penetrate the GFW
        .asString
        .body
    pprint.pprintln(GoogleFinance.getPrices(1, "UKOG", client))
  }
}
