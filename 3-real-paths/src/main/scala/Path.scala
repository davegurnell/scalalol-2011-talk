sealed trait Path {
  
  /** The type of data we're extracting from the URL. */
  type Result <: HList
  
  /** Decode a URL path into an argument list, preserving the order of the arguments. */
  final def decode(path: List[String]): Option[Result] =
    decodeReversed(path.reverse)
  
  /** Encode an HList as a URL path, preserving the order of the arguments. */
  final def encode(args: Result): List[String] =
    encodeReversed(args).reverse
  
  /**
   * Decode a URL path into an argument list, reversing the order of the arguments.
   * 
   * The reversal places PNil at the end of the URL path, which will eventually
   * provide support for rest-arguments.
   */
  def decodeReversed(path: List[String]): Option[Result]
  
  /** Encode an HList as a reversed URL path, reversing the order of the arguments.
   * 
   * The reversal places PNil at the end of the URL path, which will eventually
   * provide support for rest-arguments.
   */
  def encodeReversed(args: Result): List[String]
  
}

case class PCons[Hd, Tl <: Path](val head: Arg[Hd], val tail: Tl) extends Path {
  
  type Result = HCons[Hd, tail.Result]
  
  def decodeReversed(path: List[String]): Option[Result] =
    path match {
      case Nil => None
      case h :: t =>
        for {
          h2 <- head.decode(h)
          t2 <- tail.decode(t)
        } yield HCons(h2, t2)
    }
  
  def encodeReversed(args: Result): List[String] =
    head.encode(args.head) ::
    tail.encode(args.tail)
  
  def /[T](arg: Arg[T]) =
    PCons(arg, this)
  
}

sealed abstract class PNil extends Path {
  
  type Result = HNil
  
  def decodeReversed(path: List[String]): Option[Result] =
    path match {
      case Nil => Some(HNil)
      case _ => None
    }
  
  def encodeReversed(args: Result): List[String] = Nil
  
  def /[T](arg: Arg[T]) =
    PCons(arg, this)
  
}

case object PNil extends PNil
