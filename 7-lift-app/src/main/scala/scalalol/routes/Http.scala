package scalalol
package routes

import net.liftweb.common._
import net.liftweb.http._

trait RequestHandler {

  /**
   * Attempt to handle an HTTP Request.
   * 
   * If the URL matches an expected pattern, return Some(Response).
   *
   * If the URL cannot be matched, return None.
   */
  def dispatch(req: Req): Option[LiftResponse]
  
  /**
   * Build a Lift DispatchPF to handle matching requests.
   *
   * Note that the implementation here is very naive as it effectively
   * processes the request twice, once for isDefinedAt() and once for apply().
   *
   * It is straightforward to modify this code so that isDefinedAt
   * simply checks the structure of the URL path.
   */
  def dispatchPF: LiftRules.DispatchPF =
    new LiftRules.DispatchPF {
      
      def isDefinedAt(req: Req) =
        dispatch(req).isDefined
      
      def apply(req: Req) =
        () => Box.option2Box(dispatch(req))
      
    }

}
