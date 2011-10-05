import org.specs._

class HListSpec extends Specification {

  "HLists can have heterogeneous arguments" in {
    val list = 2.0 :: 1 :: HNil
    
    list.head mustEqual 2.0
    list.tail mustEqual HCons(1, HNil)
    
    list.tail.head mustEqual 1
    list.tail.tail mustEqual HNil
  }
  
} 
