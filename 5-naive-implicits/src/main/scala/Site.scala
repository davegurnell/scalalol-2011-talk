trait Site extends HListOps {

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

  def handleListNames(): Response =
    Response(names.mkString(", "))
  
  def handleAddName(name: String): Response = {
    names = names :+ name
    Response("added " + name)
  }
  
  def handleRemoveName(name: String): Response = {
    names = names filterNot (_ == name)
    Response("removed " + name)
  }

}

import AddressBook._

addPerson(Tuple1("Alice"))
addPerson(Tuple1("Bob"))
listNames(())

dispatch(Request("/names/add/Charlie"))
dispatch(Request("/names/add/Dave"))
dispatch(Request("/names"))

addPerson.url(HCons("Alice", HNil))
removePerson.url(HCons("Alice", HNil))

*/