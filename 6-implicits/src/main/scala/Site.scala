trait Site extends RouteBuilder {

  var routes: List[Route[_]] =
    Nil
  
  val end = PNil
  val any = PAny
  
  implicit val site = this
  
  def addRoute(route: Route[_]): Unit =
    routes = routes :+ route
  
  def dispatch(req: Request): Option[Response] = 
    dispatch(req, routes)
  
  private def dispatch(req: Request, routes: List[Route[_]]): Option[Response] =
    routes match {
      case Nil => None
      
      case head :: tail =>
        head.dispatch(req).orElse(dispatch(req, tail))
    }
}
