import org.specs._

class PathSpec extends Specification {
  
  val path = PNil / StringArg / IntArg

  "Path.encode works as expected" in {
    path.encode(HCons(123, HCons("abc", HNil))) mustEqual List("123", "abc")
  }
  
  "Path.decode works as expected" in {
    path.decode(List("123", "abc")) must beSome(HCons(123, HCons("abc", HNil)))
    path.decode(List("abc", "123")) must beNone
  }
  
}