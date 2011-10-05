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
    makeResponse("%s + %s = %s".format(a, b, ans))
  }

  def doMultiply(a: Int, b: Int): LiftResponse = {
    val ans = a * b
    makeResponse("%s * %s = %s".format(a, b, ans))
  }

  def doSquare(a: Int): LiftResponse = {
    val ans = a * a
    makeResponse("%s ^ 2 = %s".format(a, ans))
  }
  
  def doStringMultiply(a: String, b: Int): LiftResponse = {
    val ans = a * b
    makeResponse("%s * %s = %s".format(a, b, ans))
  }
  
  def doStringAppend(args: List[String]): LiftResponse = {
    val ans = args.mkString
    makeResponse("append(%s) = %s".format(args, ans))
  }
  
  def makeResponse(content: String): LiftResponse =
    InMemoryResponse(
      content.getBytes,
      List(("Content-Type", "text/plain; charset=utf-8")),
      Nil,
      200)

}