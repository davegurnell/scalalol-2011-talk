package net.liftweb.http

import net.liftweb.common._
import net.liftweb.sitemap._
import scala.xml._

trait LiftSiteExtras {

  def textResponse(content: String): LiftResponse =
    InMemoryResponse(
      content.getBytes,
      List(("Content-Type", "text/plain; charset=utf-8")),
      Nil,
      200)
  
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
