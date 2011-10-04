sealed trait Path {
  
  /** The type of the data we're extracting from the URL. */
  type Result <: HList

  /** Decode a URL path into an HList. */
  def decode(path: List[String]): Option[Result]
   
  /** Encode an HList as a URL path. */
  def encode(args: Result): List[String]
  
}

case class PCons[Hd, Tl <: Path](val head: Arg[Hd], val tail: Tl) extends Path {
  
  type Result = HCons[Hd, tail.Result]
  
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
    head.encode(args.head) ::
    tail.encode(args.tail)
  
  def /[T](arg: Arg[T]) =
    PCons(arg, this)
  
}

case object PNil extends Path {
  
  type Result = HNil
  
  def decode(path: List[String]): Option[Result] =
    path match {
      case Nil => Some(HNil)
      case _ => None
    }
  
  def encode(args: Result): List[String] = Nil
  
  def /[T](arg: Arg[T]) =
    PCons(arg, this)
  
}

/*

Try these:

val path = PNil / StringArg / IntArg
path decode List("123", "abc")
path decode List("abc", "123")

*/
