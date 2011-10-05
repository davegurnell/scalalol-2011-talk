case class Route[Res <: HList](
  val path: Path { type Result = Res },
  val fn: (Res) => Response
)(implicit val site: Site) extends RequestHandler {
  
  site.addRoute(this)
  
  def dispatch(req: Request): Option[Response] =
    path.decode(req.path).map(fn)
  
  def url(arg: Res) =
    path.encode(arg).mkString("/", "/", "")
  
  def apply(arg: Res) =
    fn.apply(arg)
  
}
