import org.specs._

class SiteSpec extends Specification {
  
  object Calculator extends Site {
  
    // Routing table:
  
    val add      = ("add"      :/: IntArg    :/: "to"   :/: IntArg  :/: end) >> (doAdd _)
    val multiply = ("multiply" :/: IntArg    :/: "by"   :/: IntArg  :/: end) >> (doMultiply _)
    val square   = ("square"   :/: IntArg                           :/: end) >> (doSquare _)
    val repeat   = ("repeat"   :/: StringArg :/: IntArg :/: "times" :/: end) >> (doRepeat _)
    val append   = ("append"                                        :/: any) >> (doAppend _)
    
    // Implementation:
  
    def doAdd(arg: HCons[Int, HCons[Int, HNil]]): Response = {
      val a = arg.head
      val b = arg.tail.head
      Response("%s + %s = %s".format(a, b, a + b))
    }
  
    def doMultiply(arg: HCons[Int, HCons[Int, HNil]]): Response = {
      val a = arg.head
      val b = arg.tail.head
      Response("%s * %s = %s".format(a, b, a * b))
    }
  
    def doSquare(arg: HCons[Int, HNil]): Response = {
      val a = arg.head
      multiply(a :: a :: HNil)
    }
    
    def doRepeat(arg: HCons[String, HCons[Int, HNil]]): Response = {
      val a = arg.head
      val b = arg.tail.head
      Response("%s * %s = %s".format(a, b, a * b))
    }
    
    def doAppend(arg: HCons[List[String], HNil]): Response = {
      val a = arg.head
      Response("append(%s) = %s".format(a, a.mkString))
    }

  }

  "site dispatches to the correct route" in {
    Calculator.dispatch(Request("/add/1/to/2"))         must beSome(Response("1 + 2 = 3"))
    Calculator.dispatch(Request("/multiply/3/by/4"))    must beSome(Response("3 * 4 = 12"))
    Calculator.dispatch(Request("/square/5"))           must beSome(Response("5 * 5 = 25"))
    Calculator.dispatch(Request("/multiply/abc/by/2"))  must beSome(Response("abc * 2 = abcabc"))
    Calculator.dispatch(Request("/append/abc/def/ghi")) must beSome(Response("append(List(abc, def, ghi)) = abcdefghi"))
    Calculator.dispatch(Request("/foo"))                must beNone
  }

  "routes can be invoked directly" in {
    Calculator.add(1 :: 2 :: HNil)                       mustEqual Response("1 + 2 = 3")
    Calculator.multiply(3 :: 4 :: HNil)                  mustEqual Response("3 * 4 = 12")
    Calculator.square(5 :: HNil)                         mustEqual Response("5 * 5 = 25")
    Calculator.repeat("abc" :: 2 :: HNil)                mustEqual Response("abc * 2 = abcabc")
    Calculator.append(List("abc", "def", "ghi") :: HNil) mustEqual Response("append(List(abc, def, ghi)) = abcdefghi")
  }
  
  "routes produce the correct urls" in {
    Calculator.add.url(1 :: 2 :: HNil)                       mustEqual "/add/1/to/2"
    Calculator.multiply.url(3 :: 4 :: HNil)                  mustEqual "/multiply/3/by/4"
    Calculator.square.url(5 :: HNil)                         mustEqual "/square/5"
    Calculator.repeat.url("abc" :: 2 :: HNil)                mustEqual "/repeat/abc/2/times"
    Calculator.append.url(List("abc", "def", "ghi") :: HNil) mustEqual "/append/abc/def/ghi"
  }
  
}
