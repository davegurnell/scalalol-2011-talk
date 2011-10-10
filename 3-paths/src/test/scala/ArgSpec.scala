import org.specs._

class ArgSpec extends Specification {
  
  "IntArg.encode encodes as expected" in {
    IntArg.encode(123) mustEqual "123"
  }
  
  "IntArg.decode decodes integers correctly" in {
    IntArg.decode("123") must beSome(123)
  }
  
  "IntArg.decode only decodes integers" in {
    IntArg.decode("abc") must beNone
  }

  "StringArg.encode does not (un)escape reserved characters" in {
    StringArg.encode("a/b") mustEqual "a/b"
    StringArg.encode("a%2Fb") mustEqual "a%2Fb"
  }
  
  "StringArg.decode does not (un)escape reserved characters" in {
    StringArg.decode("a/b") must beSome("a/b")
    StringArg.decode("a%2Fb") must beSome("a%2Fb")
  }
  
}
