import org.specs._

class HListSpec extends Specification {

  "HLists can have heterogeneous arguments" in {
    val list = 2.0 :: 1 :: HNil
    
    list.head must be_==(2.0)
    list.tail must be_==(HCons(1, HNil))
    
    list.tail.head must be_==(1)
    list.tail.tail must be_==(HNil)
  }
  
} 
