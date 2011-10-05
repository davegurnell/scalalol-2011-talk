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

  "StringArg.encode unescapes reserved characters" in {
    StringArg.encode("a/b") mustEqual "a%2Fb"
  }
  
  "StringArg.decode escapes reserved characters" in {
    StringArg.decode("a%2Fb") must beSome("a/b")
  }
  
  "LiteralArg.encode encodes to a literal" in {
    LiteralArg("anything").encode(()) mustEqual "anything"
  }
  
  "LiteralArg.decode decodes to Unit" in {
    LiteralArg("anything").decode("anything") must beSome(())
  }

  "LiteralArg.decode only decodes the correct path segment" in {
    LiteralArg("anything").decode("nothing") must beNone
  }
  
}
