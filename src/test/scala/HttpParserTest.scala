import org.pwharned.http.response.HttpResponse
import org.pwharned.http.request.HttpParser

import java.nio.ByteBuffer

object HttpParserTest {

  {
    // 1) Prepare a valid HTTP request string
    val rawRequest =
      "GET /hello HTTP/1.1\r\n" +
        "Host: example.com\r\n" +
        "User-Agent: ScalaTest\r\n" +
        "\r\n"

    // 2) Wrap bytes in a ByteBuffer and feed to parser
    val parser = new HttpParser()
    val input = ByteBuffer.wrap(rawRequest.getBytes("UTF-8"))
    parser.feed(input)

    // 3) Take the parsed view and assert its contents
    val maybeView = parser.take()
    assert(maybeView.isDefined, "Expected a parsed HttpRequestView")
    println(maybeView)
    val view = maybeView.get
    assert(view.method == "GET")
    assert(view.path == "/hello")
    assert(view.version == "HTTP/1.1")

    // 4) Check headers lazily
    val headers = view.headers
    assert(headers("Host") == "example.com")
    assert(headers("User-Agent") == "ScalaTest")
  }
}

@main
def test: Unit =
  HttpParserTest
