case class Request(val path: List[String])

object Request {
  
  def apply(path: String): Request =
    Request(path.split("/").
                 toList.
                 filterNot(_.trim == ""))
  
}

case class Response(val content: String)
