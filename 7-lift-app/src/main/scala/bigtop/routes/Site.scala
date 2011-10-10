package bigtop
package routes

import net.liftweb.http._

trait Site extends RequestHandler with RouteBuilder with LiftSiteExtras {

  var routes: List[Route[_]] =
    Nil
  
  val end = PNil
  val any = PAny
  
  implicit val site = this
  
  def addRoute(route: Route[_]): Unit =
    routes = routes :+ route
  
  def dispatch(req: Req): Option[LiftResponse] = 
    dispatch(req, routes)
  
  private def dispatch(req: Req, routes: List[Route[_]]): Option[LiftResponse] =
    routes match {
      case Nil => None
      
      case head :: tail =>
        head.dispatch(req).orElse(dispatch(req, tail))
    }
}
