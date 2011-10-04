import java.net.URLEncoder.{encode => urlEncode}
import java.net.URLDecoder.{decode => urlDecode}

/**
 * Trait describing the required behaviour to map URL path fragments
 * to typed values we can use in our applications.
 *
 * For example, the URL:
 * 
 *   http://example.com/abc/123
 *
 * has a path "/abc/123" containing two fragments: "abc" and "123".
 *
 * You can subclass this for any T: String, Int, Person, etc.
 */
trait Arg[T] {

  /**
   * Attempt to decode a URL path fragment into a typed value.
   * Return Some(value) if successful, None if unsuccessful.
   */
  def decode(in: String): Option[T]
  
  /**
   * Encode a typed value as a URL path fragment/
   */
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
