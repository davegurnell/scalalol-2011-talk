package net.liftweb.http

import net.liftweb.common._
import net.liftweb.sitemap._
import scala.xml._

/**
 * Proof-of-concept adapter to make a Site "compatible" 
 * with LiftRules.dispatch().
 *
 * ==================== NOTE ====================
 * This code relies on a couple of nasty hacks, 
 * and should *not* be considered production-ready:
 *
 *  - LiftSiteExtras is defined in the net.liftweb.http package
 *    so it has access to LiftSession.processTemplate() and
 *    S.CurrentLocation;
 *
 *  - templateResponse() generates a dummy Loc and 
 *    registers it in S.CurrentLocation to give
 *    Snippets access to the locValue argument.
 * ==============================================
 */
trait LiftSiteExtras {

  /** Generate a plain-text HTTP response with the supplied content. */
  def textResponse(content: String): LiftResponse =
    InMemoryResponse(
      content.getBytes,
      List(("Content-Type", "text/plain; charset=utf-8")),
      Nil,
      200)
  
  /**
   * Invoke the supplied template and return a an HTTP response
   * containing the final HTML.
   *
   * Returns a NotFoundResponse if the template cannot be found.
   * 
   * The current "Loc value" is set to locValue
   * so that Snippets may access it.
   */
  def templateResponse[T](locValue: T, templatePath: List[String]): LiftResponse = {
    val response: Box[LiftResponse] =
      S.request.flatMap { request =>
        S.session.flatMap { session =>
          import Loc._
    
          val requestPath: List[String] =
            request.path.wholePath
    
          val requestPathString: String =
            requestPath.mkString("/", "/", "")
  
          val loc =
            new Loc[T] {
              val name = requestPathString
              val link = new Link(requestPath)
              val text = new LinkText((t: T) => Text(requestPathString))
              val params = Nil
              val defaultValue = Full(locValue)
              
              override def allParams = Nil
            }
  
          S.CurrentLocation.doWith(Full(loc)) {
            session.processTemplate(Templates(templatePath), S.request.open_!, S.request.open_!.path, 200)
          }
        }
      }
    
    response.getOrElse(NotFoundResponse("Guess the template couldn't be found?"))
  }

}
