case class Request(val path: List[String])

object Request {
  
  def apply(path: String): Request =
    Request(path.split("/").
                 toList.
                 filterNot(_.trim == ""))
  
}

case class Response(val content: String)

trait RequestHandler {

  /**
   * Attempt to handle an HTTP Request.
   * 
   * If the URL matches an expected pattern, return Some(Response).
   *
   * If the URL cannot be matched, return None.
   */
  def dispatch(req: Request): Option[Response]

}
