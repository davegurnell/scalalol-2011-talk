trait Site {

  var routes: List[Route[_]] =
    Nil
  
  val root = PNil
  
  implicit val site = this
  
  def add(route: Route[_]): Unit =
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

object AddressBook extends Site {
  
  // Routing table:
  
  val listNames    = root / "names" >> (handleListNames _)
  val addPerson    = root / "names" / "add" / StringArg >> (handleAddName _)
  val removePerson = root / "names" / StringArg / "remove" >> (handleRemoveName _)
  
  // Implementation:
  
  var names: List[String] = Nil

  def handleListNames(arg: HNil): Response =
    Response(names.mkString(", "))
  
  def handleAddName(arg: HCons[String, HNil]): Response = {
    names = names :+ arg.head
    Response("added " + arg.head)
  }
  
  def handleRemoveName(arg: HCons[String, HNil]): Response = {
    names = names filterNot (_ == arg.head)
    Response("removed " + arg.head)
  }

}

AddressBook.addPerson(HCons("Alice", HNil))
AddressBook.addPerson(HCons("Bob", HNil))
AddressBook.listNames(HNil)

AddressBook.dispatch(Request("/names/add/Charlie"))
AddressBook.dispatch(Request("/names/add/Dave"))
AddressBook.dispatch(Request("/names"))

AddressBook.addPerson.url(HCons("Alice", HNil))
AddressBook.removePerson.url(HCons("Alice", HNil))

*/