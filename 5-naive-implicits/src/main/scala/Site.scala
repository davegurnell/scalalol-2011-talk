trait Site extends HListOps {

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

/*

Try these:

object Calculator extends Site {
  
  // Routing table:
  
  val add      = root / "add"      / IntArg / "to" / IntArg >> (doAdd _)
  val multiply = root / "multiply" / IntArg / "by" / IntArg >> (doMultiply _)
  val square   = root / "square"   / IntArg                 >> (doSquare _)
  
  // Implementation:
  
  def doAdd(a: Int, b: Int): Response = {
    val ans = a + b
    Response("%s + %s = %s".format(a, b, ans))
  }
  
  def doMultiply(a: Int, b: Int): Response = {
    val ans = a * b
    Response("%s * %s = %s".format(a, b, ans))
  }
  
  def doSquare(a: Int): Response = {
    val ans = a * a
    Response("%s ^ 2 = %s".format(a, ans))
  }

}

Calculator.add((1, 2))
Calculator.multiply((3, 4))
Calculator.square(Tuple1(5))

import Calculator._
Calculator.add((1, 2))
Calculator.multiply((3, 4))
Calculator.square(Tuple1(5))

Calculator.dispatch(Request("/add/1/to/2"))
Calculator.dispatch(Request("/multiply/3/by/4"))
Calculator.dispatch(Request("/square/5"))

Calculator.add.url((1, 2))
Calculator.multiply.url((3, 4))
Calculator.square.url(Tuple1(5))

*/