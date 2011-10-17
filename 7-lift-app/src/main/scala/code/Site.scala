package code

import net.liftweb.http._
import bigtop.routes._

object Site extends bigtop.routes.Site {

  // Routing table:

  val add      = ("add"      :/: IntArg    :/: "to"   :/: IntArg  :/: end) >> (doAdd _)
  val multiply = ("multiply" :/: IntArg    :/: "by"   :/: IntArg  :/: end) >> (doMultiply _)
  val square   = ("square"   :/: IntArg                           :/: end) >> (doSquare _)
  val repeat   = ("multiply" :/: StringArg :/: IntArg :/: "times" :/: end) >> (doRepeat _)
  val append   = ("append"                                        :/: any) >> (doAppend _)
  
  // Implementation:

  def doAdd(a: Int, b: Int) =
    templateResponse(Calculation("+", List(a, b), a + b), "calculation" :: Nil)

  def doMultiply(a: Int, b: Int) =
    templateResponse(Calculation("*", List(a, b), a * b), "calculation" :: Nil)

  def doSquare(a: Int) =
    multiply(a, a)
  
  def doRepeat(a: String, b: Int) =
    templateResponse(Calculation("repeat", List(a, b), a * b), "calculation" :: Nil)
  
  def doAppend(args: List[String]) =
    templateResponse(Calculation("append", args, args.mkString), "calculation" :: Nil)
  
}
