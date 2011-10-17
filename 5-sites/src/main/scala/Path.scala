sealed trait Path {
  
  /** The type of data we're extracting from the path, i.e. ignoring segments that aren't arguments. */
  type Result <: HList
  
  /** Decode a URL path into an argument list. */
  def decode(path: List[String]): Option[Result]
  
  /** Encode an HList as a URL path. */
  def encode(args: Result): List[String]
  
  /**
   * Create a Route from this Path, the supplied HList function, and the supplied Site.
   * The Route's constructor automcatically registers the Route with the Site.
   */
  def >>(fn: (Result) => Response)(implicit site: Site): Route[Result] =
    Route(site, this, fn)
  
}

case class PLiteral[T <: Path](val head: String, val tail: T) extends Path {
  
  type Result = tail.Result
  
  def decode(path: List[String]): Option[Result] =
    path match {
      case Nil => None
      case h :: t =>
        if(h == head) tail.decode(t) else None
    }
    
  def encode(args: Result): List[String] =
    head :: tail.encode(args)
  
  def :/:(arg: String) =
    PLiteral(arg, this)
  
  def :/:[T](arg: Arg[T]) =
    PMatch(arg, this)
  
}

case class PMatch[H, T <: Path](val head: Arg[H], val tail: T) extends Path {
  
  type Result = HCons[H, tail.Result]
  
  def decode(path: List[String]): Option[Result] =
    path match {
      case Nil => None
      case h :: t =>
        for {
          h2 <- head.decode(h)
          t2 <- tail.decode(t)
        } yield HCons(h2, t2)
    }
  
  def encode(args: Result): List[String] =
    head.encode(args.head) :: tail.encode(args.tail)
  
  def :/:(arg: String) =
    PLiteral(arg, this)
  
  def :/:[T](arg: Arg[T]) =
    PMatch(arg, this)
  
}

sealed abstract class PNil extends Path {
  
  type Result = HNil
  
  def decode(path: List[String]): Option[Result] =
    path match {
      case Nil => Some(HNil)
      case _ => None
    }
  
  def encode(args: Result): List[String] =
    Nil
  
  def :/:(arg: String) =
    PLiteral(arg, this)
  
  def :/:[T](arg: Arg[T]) =
    PMatch(arg, this)
  
}

case object PNil extends PNil

sealed abstract class PAny extends Path {
  
  type Result = HCons[List[String], HNil]
  
  def decode(path: List[String]): Option[Result] =
    Some(HCons(path, HNil))
  
  def encode(args: Result): List[String] =
    args.head
  
  def :/:(arg: String) =
    PLiteral(arg, this)
  
  def :/:[T](arg: Arg[T]) =
    PMatch(arg, this)
  
}

case object PAny extends PAny
