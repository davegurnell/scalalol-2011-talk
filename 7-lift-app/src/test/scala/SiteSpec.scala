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
  
    def doAdd(a: Int, b: Int): Response = {
      val ans = a + b
      Response("%s + %s = %s".format(a, b, ans))
    }
  
    def doMultiply(a: Int, b: Int): Response = {
      val ans = a * b
      Response("%s * %s = %s".format(a, b, ans))
    }
  
    def doSquare(a: Int): Response = {
      val ans = a * a
      Response("%s ^ 2 = %s".format(a, ans))
    }
    
    def doStringMultiply(a: String, b: Int): Response = {
      val ans = a * b
      Response("%s * %s = %s".format(a, b, ans))
    }
    
    def doStringAppend(args: List[String]): Response = {
      val ans = args.mkString
      Response("append(%s) = %s".format(args, ans))
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
    import HListOps._
    
    Calculator.add(1, 2)                               mustEqual Response("1 + 2 = 3")
    Calculator.multiply(3, 4)                          mustEqual Response("3 * 4 = 12")
    Calculator.square(5)                               mustEqual Response("5 ^ 2 = 25")
    Calculator.stringMultiply("abc", 2)                mustEqual Response("abc * 2 = abcabc")
    Calculator.stringAppend(List("abc", "def", "ghi")) mustEqual Response("append(List(abc, def, ghi)) = abcdefghi")
  }
  
  "routes produce the correct urls" in {
    import HListOps._
    
    Calculator.add.url(1, 2)                               mustEqual "/add/1/to/2"
    Calculator.multiply.url(3, 4)                          mustEqual "/multiply/3/by/4"
    Calculator.square.url(5)                               mustEqual "/square/5"
    Calculator.stringMultiply.url("abc", 2)                mustEqual "/multiply/abc/by/2"
    Calculator.stringAppend.url(List("abc", "def", "ghi")) mustEqual "/append/abc/def/ghi"
  }

  
}
