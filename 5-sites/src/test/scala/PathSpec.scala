import org.specs._

class PathSpec extends Specification {
  
  val nilPath = "a" :/: StringArg :/: "b" :/: IntArg :/: "c" :/: PNil
  
  "Nil-terminated path: decode works as expected" in {
    nilPath.decode(List("abc", "123")) must beNone
    nilPath.decode(List("a", "abc", "b", "123", "c")) must beSome("abc" :: 123 :: HNil)
    nilPath.decode(List("a", "123", "b", "abc", "c")) must beNone
    nilPath.decode(List("c", "123", "b", "abc", "a")) must beNone
  }
  
  "Nil-terminated path: encode works as expected" in {
    nilPath.encode("abc" :: 123 :: HNil) mustEqual List("a", "abc", "b", "123", "c")
  }
   
  val restPath = "a" :/: StringArg :/: "b" :/: IntArg :/: "c" :/: PAny
   
  "Any-terminated path: decode works as expected" in {
    restPath.decode(List("abc", "123")) must beNone
    restPath.decode(List("a", "abc", "b", "123", "c")) must beSome("abc" :: 123 :: List() :: HNil)
    restPath.decode(List("a", "abc", "b", "123", "c", "d", "e")) must beSome("abc" :: 123 :: List("d", "e") :: HNil)
    restPath.decode(List("a", "123", "b", "abc", "c")) must beNone
    restPath.decode(List("c", "123", "b", "abc", "a")) must beNone
  }
  
  "Any-terminated path: encode works as expected" in {
    restPath.encode("abc" :: 123 :: List[String]() :: HNil) mustEqual List("a", "abc", "b", "123", "c")
    restPath.encode("abc" :: 123 :: List("d", "e") :: HNil) mustEqual List("a", "abc", "b", "123", "c", "d", "e")
  }
  
}
