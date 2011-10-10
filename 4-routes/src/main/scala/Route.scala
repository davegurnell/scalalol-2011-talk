case class Route[Res <: HList](
  val path: Path { type Result = Res },
  val fn: (Res) => Response
) {
  
  def dispatch(req: Request): Option[Response] =
    path.decode(req.path).map(fn)
  
  def url(arg: Res) =
    Request.createUrl(path.encode(arg))
  
  def apply(arg: Res) =
    fn.apply(arg)
  
}
