package code

import net.liftweb.http._
import scalalol.routes._

object Site extends scalalol.routes.Site {

  // Routing table:

  val add            = ("add"      :/: IntArg    :/: "to" :/: IntArg :/: end) >> (doAdd _)
  val multiply       = ("multiply" :/: IntArg    :/: "by" :/: IntArg :/: end) >> (doMultiply _)
  val square         = ("square"   :/: IntArg                        :/: end) >> (doSquare _)

  val stringMultiply = ("multiply" :/: StringArg :/: "by" :/: IntArg :/: end) >> (doStringMultiply _)
  val stringAppend   = ("append"                                     :/: any) >> (doStringAppend _)
  
  // Implementation:

  def doAdd(a: Int, b: Int): LiftResponse = {
    val ans = a + b
    templateResponse(Calculation("+", List(a, b), ans), "calculation" :: Nil)
  }

  def doMultiply(a: Int, b: Int): LiftResponse = {
    val ans = a * b
    templateResponse(Calculation("*", List(a, b), ans), "calculation" :: Nil)
  }

  def doSquare(a: Int): LiftResponse = {
    val ans = a * a
    templateResponse(Calculation("square", List(a), ans), "calculation" :: Nil)
  }
  
  def doStringMultiply(a: String, b: Int): LiftResponse = {
    val ans = a * b
    templateResponse(Calculation("stringMultiply", List(a, b), ans), "calculation" :: Nil)
  }
  
  def doStringAppend(args: List[String]): LiftResponse = {
    val ans = args.mkString
    templateResponse(Calculation("stringAppend", args, ans), "calculation" :: Nil)
  }
  
}