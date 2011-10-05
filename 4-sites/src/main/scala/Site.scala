trait Site {

  var routes: List[Route[_]] =
    Nil
  
  val root = PNil
  
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
  
  def doAdd(arg: HCons[Int, HCons[Int, HNil]]): Response = {
    val a = arg.head
    val b = arg.tail.head
    val ans = a + b
    Response("%s + %s = %s".format(a, b, ans))
  }
  
  def doMultiply(arg: HCons[Int, HCons[Int, HNil]]): Response = {
    val a = arg.head
    val b = arg.tail.head
    val ans = a * b
    Response("%s * %s = %s".format(a, b, ans))
  }
  
  def doSquare(arg: HCons[Int, HNil]): Response = {
    val a = arg.head
    val ans = a * a
    Response("%s ^ 2 = %s".format(a, ans))
  }

}

Calculator.add(HCons(2, HCons(1, HNil)))
Calculator.multiply(HCons(4, HCons(3, HNil)))
Calculator.square(HCons(5, HNil))

Calculator.dispatch(Request("/add/1/to/2"))
Calculator.dispatch(Request("/multiply/3/by/4"))
Calculator.dispatch(Request("/square/5"))

Calculator.add.url(HCons(2, HCons(1, HNil)))
Calculator.multiply.url(HCons(4, HCons(3, HNil)))
Calculator.square.url(HCons(5, HNil))

*/