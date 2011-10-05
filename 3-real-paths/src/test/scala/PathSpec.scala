import org.specs._

class PathSpec extends Specification {
  
  val path = StringArg :/: IntArg :/: PNil
    
  "Path.encode works as expected" in {
    path.encode(HCons("abc", HCons(123, HNil))) mustEqual List("abc", "123")
  }
  
  "Path.decode works as expected" in {
    path.decode(List("abc", "123")) must beSome(HCons("abc", HCons(123, HNil)))
    path.decode(List("123", "abc")) must beNone
  }
  
}
