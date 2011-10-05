import java.net.URLEncoder.{encode => urlEncode}
import java.net.URLDecoder.{decode => urlDecode}

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
  
  /** Decode a URL path into an argument list. */
  def decode(path: List[String]): Option[Result]
  
  /** Encode an HList as a URL path. */
  def encode(args: Result): List[String]
  
}

sealed trait PCons[H, T <: Path] extends Path {
  
  def head: Arg[H]
  def tail: T
  
}

case class PLiteral[T <: Path](headString: String, val tail: T) extends PCons[Unit, T] {
  
  type Head = Unit
  type Tail = T
  type Result = tail.Result
  
  val head: Arg[Unit] =
    LiteralArg(headString)
  
  def decode(path: List[String]): Option[Result] =
    path match {
      case Nil => None
      case h :: t =>
        for {
          h2 <- head.decode(h)
          t2 <- tail.decode(t)
        } yield t2
    }
    
  def encode(args: Result): List[String] =
    head.encode(()) :: tail.encode(args)
  
  def :/:(arg: String) =
    PLiteral(arg, this)
  
  def :/:[T](arg: Arg[T]) =
    PArg(arg, this)
  
}

case class PArg[H, T <: Path](val head: Arg[H], val tail: T) extends PCons[H, T] {
  
  type Head = Arg[H]
  type Tail = T
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
    PArg(arg, this)
  
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
    PArg(arg, this)
  
}

case object PNil extends PNil

sealed abstract class PAny extends Path {
  
  type Result = HCons[List[String], HNil]
  
  def decode(path: List[String]): Option[Result] =
    Some(HCons(path.map(str => urlDecode(str, "utf-8")), HNil))
  
  def encode(args: Result): List[String] =
    args.head.map(str => urlEncode(str, "utf-8"))
  
  def :/:(arg: String) =
    PLiteral(arg, this)
  
  def :/:[T](arg: Arg[T]) =
    PArg(arg, this)
  
}

case object PAny extends PAny

/*

Try this:

val path = "add" :/: IntArg :/ :"to" :/: IntArg :/: PNil

path.decode(List("a", "abc", "b", "123", "c"))

path.encode(HCons("abc", HCons(123, HNil)))

*/