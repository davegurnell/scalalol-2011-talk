import java.net.URLEncoder.{encode => urlEncode}
import java.net.URLDecoder.{decode => urlDecode}

case class Request(val path: List[String])

object Request {
  
  def apply(path: String): Request =
    Request(path.split("/").
                 toList.
                 filterNot(_.trim == "").
                 map(urlDecode(_, "utf-8")))
  
  def createUrl(path: Seq[String]): String =
    path.map(urlEncode(_, "utf-8")).mkString("/", "/", "")
  
}

case class Response(val content: String)
