package bigtop
package routes

trait HListOps {
  
  implicit def hlistFunction0[A, Res](fn: () => Res) =
    new Function1[HNil, Res] {
      def apply(in: HNil): Res = {
        fn()
      }
    }
  
  implicit def hlistFunction1[A, Res](fn: (A) => Res) =
    new Function1[HCons[A, HNil], Res] {
      def apply(in: HCons[A, HNil]): Res = {
        val h1 = in.head
        
        fn(h1)
      }
    }
  
  implicit def hlistFunction2[A, B, Res](fn: (A, B) => Res) =
    new Function1[HCons[A, HCons[B, HNil]], Res] {
      def apply(in: HCons[A, HCons[B, HNil]]): Res = {
        val h1 = in.head
        val t1 = in.tail
        val h2 = t1.head
        
        fn(h1, h2)
      }
    }
  
  implicit def hlistFunction3[A, B, C, Res](fn: (A, B, C) => Res) =
    new Function1[HCons[A, HCons[B, HCons[C, HNil]]], Res] {
      def apply(in: HCons[A, HCons[B, HCons[C, HNil]]]): Res = {
        val h1 = in.head
        val t1 = in.tail
        val h2 = t1.head
        val t2 = t1.tail
        val h3 = t2.head
        
        fn(h1, h2, h3)
      }
    }
  
  implicit def hlistFunction4[A, B, C, D, Res](fn: (A, B, C, D) => Res) =
    new Function1[HCons[A, HCons[B, HCons[C, HCons[D, HNil]]]], Res] {
      def apply(in: HCons[A, HCons[B, HCons[C, HCons[D, HNil]]]]): Res = {
        val h1 = in.head
        val t1 = in.tail
        val h2 = t1.head
        val t2 = t1.tail
        val h3 = t2.head
        val t3 = t2.tail
        val h4 = t3.head
        
        fn(h1, h2, h3, h4)
      }
    }
  
  implicit def hlistFunction5[A, B, C, D, E, Res](fn: (A, B, C, D, E) => Res) =
    new Function1[HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HNil]]]]], Res] {
      def apply(in: HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HNil]]]]]): Res = {
        val h1 = in.head
        val t1 = in.tail
        val h2 = t1.head
        val t2 = t1.tail
        val h3 = t2.head
        val t3 = t2.tail
        val h4 = t3.head
        val t4 = t3.tail
        val h5 = t4.head
        
        fn(h1, h2, h3, h4, h5)
      }
    }
  
}

object HListOps extends HListOps
