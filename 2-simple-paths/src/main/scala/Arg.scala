import java.net.URLEncoder.{encode => urlEncode}
import java.net.URLDecoder.{decode => urlDecode}

trait Arg[T] {

  def decode(in: String): Option[T]
  
  def encode(in: T): String
  
}

case object IntArg extends Arg[Int] {
  
  def encode(value: Int) =
    urlEncode(value.toString, "utf-8")
  
  def decode(path: String) =
    try {
      Some(urlDecode(path, "utf-8").toInt)
    } catch {
      case exn: NumberFormatException => None
    }
  
}

case object StringArg extends Arg[String] {
  
  def encode(value: String) =
    urlEncode(value, "utf-8")
  
  def decode(path: String) =
    Some(urlDecode(path, "utf-8"))
  
}

/*

Try these:

StringArg decode "123"
IntArg    decode "123"

StringArg decode "abc"
IntArg    decode "abc"

*/
