import org.specs._

class SiteSpec extends Specification {
  
  object Calculator extends Site {
  
    // Routing table:
  
    val add            = ("add"      :/: IntArg    :/: "to" :/: IntArg :/: end) >> (doAdd _)
    val multiply       = ("multiply" :/: IntArg    :/: "by" :/: IntArg :/: end) >> (doMultiply _)
    val square         = ("square"   :/: IntArg                        :/: end) >> (doSquare _)

    val stringMultiply = ("multiply" :/: StringArg :/: "by" :/: IntArg :/: end) >> (doStringMultiply _)
    val stringAppend   = ("append"                                     :/: any) >> (doStringAppend _)
    
    // Implementation:
  
    def doAdd(arg: HCons[Int, HCons[Int, HNil]]): Response = {
      val a = arg.head
      val b = arg.tail.head
      val ans = a + b
      Response("%s + %s = %s".format(a, b, ans))
    }
  
    def doMultiply(arg: HCons[Int, HCons[Int, HNil]]): Response = {
      val a = arg.head
      val b = arg.tail.head
      val ans = a * b
      Response("%s * %s = %s".format(a, b, ans))
    }
  
    def doSquare(arg: HCons[Int, HNil]): Response = {
      val a = arg.head
      val ans = a * a
      Response("%s ^ 2 = %s".format(a, ans))
    }
    
    def doStringMultiply(arg: HCons[String, HCons[Int, HNil]]): Response = {
      val a = arg.head
      val b = arg.tail.head
      val ans = a * b
      Response("%s * %s = %s".format(a, b, ans))
    }
    
    def doStringAppend(arg: HCons[List[String], HNil]): Response = {
      val a = arg.head
      val ans = a.mkString
      Response("append(%s) = %s".format(a, ans))
    }

  }

  "site dispatches to the correct route" in {
    Calculator.dispatch(Request("/add/1/to/2"))         must beSome(Response("1 + 2 = 3"))
    Calculator.dispatch(Request("/multiply/3/by/4"))    must beSome(Response("3 * 4 = 12"))
    Calculator.dispatch(Request("/square/5"))           must beSome(Response("5 ^ 2 = 25"))
    Calculator.dispatch(Request("/multiply/abc/by/2"))  must beSome(Response("abc * 2 = abcabc"))
    Calculator.dispatch(Request("/append/abc/def/ghi")) must beSome(Response("append(List(abc, def, ghi)) = abcdefghi"))
    Calculator.dispatch(Request("/foo"))                must beNone
  }

  "routes can be invoked directly" in {
    Calculator.add(HCons(1, HCons(2, HNil)))                        mustEqual Response("1 + 2 = 3")
    Calculator.multiply(HCons(3, HCons(4, HNil)))                   mustEqual Response("3 * 4 = 12")
    Calculator.square(HCons(5, HNil))                               mustEqual Response("5 ^ 2 = 25")
    Calculator.stringMultiply(HCons("abc", HCons(2, HNil)))         mustEqual Response("abc * 2 = abcabc")
    Calculator.stringAppend(HCons(List("abc", "def", "ghi"), HNil)) mustEqual Response("append(List(abc, def, ghi)) = abcdefghi")
  }
  
  "routes produce the correct urls" in {
    Calculator.add.url(HCons(1, HCons(2, HNil)))                        mustEqual "/add/1/to/2"
    Calculator.multiply.url(HCons(3, HCons(4, HNil)))                   mustEqual "/multiply/3/by/4"
    Calculator.square.url(HCons(5, HNil))                               mustEqual "/square/5"
    Calculator.stringMultiply.url(HCons("abc", HCons(2, HNil)))         mustEqual "/multiply/abc/by/2"
    Calculator.stringAppend.url(HCons(List("abc", "def", "ghi"), HNil)) mustEqual "/append/abc/def/ghi"
  }

  
}
