import java.net.URLEncoder.{encode => urlEncode}
import java.net.URLDecoder.{decode => urlDecode}

sealed trait Path {
  
  /** The type of data we're extracting from the path, i.e. ignoring segments that aren't arguments. */
  type Result <: HList
  
  /** Decode a URL path into an argument list. */
  def decode(path: List[String]): Option[Result]
  
  /** Encode an HList as a URL path. */
  def encode(args: Result): List[String]
  
}

case class PLiteral[T <: Path](rawHead: String, val tail: T) extends Path {
  
  type Result = tail.Result
  
  val head: String =
    urlEncode(rawHead, "utf-8")
  
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
    PArg(arg, this)
  
}

case class PArg[H, T <: Path](val head: Arg[H], val tail: T) extends Path {
  
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
