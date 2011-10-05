sealed trait Path {
  
  /**
   * The type of the argument being captured in the head of the path,
   * or Unit if we're ignoring the head segment.
   */
  type Head
  
  /** The type of the remainder of the path. */
  type Tail <: Path
  
  /** The type of data we're extracting from the path, i.e. ignoring segments that aren't arguments. */
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

sealed trait PCons[H, T <: Path] extends Path {
  
  def head: Arg[H]
  def tail: T
  
}

case class PLiteral[T <: Path](val head: LiteralArg, val tail: T) extends PCons[Unit, T] {
  
  type Head = Unit
  type Tail = T
  type Result = tail.Result
  
  def decodeReversed(path: List[String]): Option[Result] =
    path match {
      case Nil => None
      case h :: t =>
        for {
          h2 <- head.decode(h)
          t2 <- tail.decode(t)
        } yield t2
    }
    
  def encodeReversed(args: Result): List[String] =
    head.encode(()) ::
    tail.encode(args)
  
  def /(arg: String) =
    PLiteral(LiteralArg(arg), this)
  
  def /[T](arg: Arg[T]) =
    PArg(arg, this)
  
}

case class PArg[H, T <: Path](val head: Arg[H], val tail: T) extends PCons[H, T] {
  
  type Head = Arg[H]
  type Tail = T
  type Result = HCons[H, tail.Result]
  
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
  
  def /(arg: String) =
    PLiteral(LiteralArg(arg), this)
  
  def /[T](arg: Arg[T]) =
    PArg(arg, this)
  
}

sealed abstract class PNil extends Path {
  
  type Result = HNil
  
  def decodeReversed(path: List[String]): Option[Result] =
    path match {
      case Nil => Some(HNil)
      case _ => None
    }
  
  def encodeReversed(args: Result): List[String] = Nil
  
  def /(arg: String) =
    PLiteral(LiteralArg(arg), this)
  
  def /[T](arg: Arg[T]) =
    PArg(arg, this)
  
}

case object PNil extends PNil

/*

Try this:

val path = PNil / "add" / IntArg / "to" / IntArg

*/