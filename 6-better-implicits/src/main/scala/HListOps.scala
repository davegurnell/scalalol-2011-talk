trait HListOps {
  
  implicit def unitToHList(t: Unit): HNil =
    HNil

  implicit def hlistToTuple1[A](v: HCons[A, HNil]): Tuple1[A] =
    Tuple1(v.head)
  
  implicit def tuple1ToHList[A](t: Tuple1[A]): HCons[A, HNil] =
    HCons(t._1, HNil)

  implicit def hlistToTuple2[A, B](v: HCons[A, HCons[B, HNil]]): Tuple2[B, A] = {
    val h1 = v.head
    val t1 = v.tail
    val h2 = t1.head
 
    Tuple2(h2, h1)
  }
  
  implicit def tuple2ToHList[A, B](t: Tuple2[B, A]): HCons[A, HCons[B, HNil]] =
    HCons(t._2, HCons(t._1, HNil))
  
  implicit def hlistToTuple3[A, B, C](v: HCons[A, HCons[B, HCons[C, HNil]]]): Tuple3[C, B, A] = {
    val h1 = v.head
    val t1 = v.tail
    val h2 = t1.head
    val t2 = t1.tail
    val h3 = t2.head
 
    Tuple3(h3, h2, h1)
  }
  
  implicit def tuple3ToHList[A, B, C](t: Tuple3[C, B, A]): HCons[A, HCons[B, HCons[C, HNil]]] =
    HCons(t._3, HCons(t._2, HCons(t._1, HNil)))

  implicit def hlistToTuple4[A, B, C, D](v: HCons[A, HCons[B, HCons[C, HCons[D, HNil]]]]): Tuple4[D, C, B, A] = {
    val h1 = v.head
    val t1 = v.tail
    val h2 = t1.head
    val t2 = t1.tail
    val h3 = t2.head
    val t3 = t2.tail
    val h4 = t3.head
 
    Tuple4(h4, h3, h2, h1)
  }
  
  implicit def tuple4ToHList[A, B, C, D](t: Tuple4[D, C, B, A]): HCons[A, HCons[B, HCons[C, HCons[D, HNil]]]] =
    HCons(t._4, HCons(t._3, HCons(t._2, HCons(t._1, HNil))))

  implicit def hlistToTuple5[A, B, C, D, E](v: HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HNil]]]]]): Tuple5[E, D, C, B, A] = {
    val h1 = v.head
    val t1 = v.tail
    val h2 = t1.head
    val t2 = t1.tail
    val h3 = t2.head
    val t3 = t2.tail
    val h4 = t3.head
    val t4 = t3.tail
    val h5 = t4.head
 
    Tuple5(h5, h4, h3, h2, h1)
  }

  implicit def tuple5ToHList[A, B, C, D, E](t: Tuple5[E, D, C, B, A]): HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HNil]]]]] =
    HCons(t._5, HCons(t._4, HCons(t._3, HCons(t._2, HCons(t._1, HNil)))))

  implicit def hlistToTuple6[A, B, C, D, E, F](v: HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HCons[F, HNil]]]]]]): Tuple6[F, E, D, C, B, A] = {
    val h1 = v.head
    val t1 = v.tail
    val h2 = t1.head
    val t2 = t1.tail
    val h3 = t2.head
    val t3 = t2.tail
    val h4 = t3.head
    val t4 = t3.tail
    val h5 = t4.head
    val t5 = t4.tail
    val h6 = t5.head
 
    Tuple6(h6, h5, h4, h3, h2, h1)
  }

  implicit def tuple6ToHList[A, B, C, D, E, F](t: Tuple6[F, E, D, C, B, A]): HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HCons[F, HNil]]]]]] =
    HCons(t._6, HCons(t._5, HCons(t._4, HCons(t._3, HCons(t._2, HCons(t._1, HNil))))))

  implicit def hlistToTuple7[A, B, C, D, E, F, G](v: HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HCons[F, HCons[G, HNil]]]]]]]): Tuple7[G, F, E, D, C, B, A] = {
    val h1 = v.head
    val t1 = v.tail
    val h2 = t1.head
    val t2 = t1.tail
    val h3 = t2.head
    val t3 = t2.tail
    val h4 = t3.head
    val t4 = t3.tail
    val h5 = t4.head
    val t5 = t4.tail
    val h6 = t5.head
    val t6 = t5.tail
    val h7 = t6.head
 
    Tuple7(h7, h6, h5, h4, h3, h2, h1)
  }

  implicit def tuple7ToHList[A, B, C, D, E, F, G](t: Tuple7[G, F, E, D, C, B, A]): HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HCons[F, HCons[G, HNil]]]]]]] =
    HCons(t._7, HCons(t._6, HCons(t._5, HCons(t._4, HCons(t._3, HCons(t._2, HCons(t._1, HNil)))))))

  // Functions to HList functions ---------------
  
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
  
  implicit def hlistFunction2[A, B, Res](fn: (B, A) => Res) =
    new Function1[HCons[A, HCons[B, HNil]], Res] {
      def apply(in: HCons[A, HCons[B, HNil]]): Res = {
        val h1 = in.head
        val t1 = in.tail
        val h2 = t1.head
        
        fn(h2, h1)
      }
    }
  
  implicit def hlistFunction3[A, B, C, Res](fn: (C, B, A) => Res) =
    new Function1[HCons[A, HCons[B, HCons[C, HNil]]], Res] {
      def apply(in: HCons[A, HCons[B, HCons[C, HNil]]]): Res = {
        val h1 = in.head
        val t1 = in.tail
        val h2 = t1.head
        val t2 = t1.tail
        val h3 = t2.head
        
        fn(h3, h2, h1)
      }
    }
  
  implicit def hlistFunction4[A, B, C, D, Res](fn: (D, C, B, A) => Res) =
    new Function1[HCons[A, HCons[B, HCons[C, HCons[D, HNil]]]], Res] {
      def apply(in: HCons[A, HCons[B, HCons[C, HCons[D, HNil]]]]): Res = {
        val h1 = in.head
        val t1 = in.tail
        val h2 = t1.head
        val t2 = t1.tail
        val h3 = t2.head
        val t3 = t2.tail
        val h4 = t3.head
        
        fn(h4, h3, h2, h1)
      }
    }
  
  implicit def hlistFunction5[A, B, C, D, E, Res](fn: (E, D, C, B, A) => Res) =
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
        
        fn(h5, h4, h3, h2, h1)
      }
    }
  
  implicit def hlistFunction6[A, B, C, D, E, F, Res](fn: (F, E, D, C, B, A) => Res) =
    new Function1[HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HCons[F, HNil]]]]]], Res] {
      def apply(in: HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HCons[F, HNil]]]]]]): Res = {
        val h1 = in.head
        val t1 = in.tail
        val h2 = t1.head
        val t2 = t1.tail
        val h3 = t2.head
        val t3 = t2.tail
        val h4 = t3.head
        val t4 = t3.tail
        val h5 = t4.head
        val t5 = t4.tail
        val h6 = t5.head
        
        fn(h6, h5, h4, h3, h2, h1)
      }
    }
  
  implicit def hlistFunction7[A, B, C, D, E, F, G, Res](fn: (G, F, E, D, C, B, A) => Res) =
    new Function1[HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HCons[F, HCons[G, HNil]]]]]]], Res] {
      def apply(in: HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HCons[F, HCons[G, HNil]]]]]]]): Res = {
        val h1 = in.head
        val t1 = in.tail
        val h2 = t1.head
        val t2 = t1.tail
        val h3 = t2.head
        val t3 = t2.tail
        val h4 = t3.head
        val t4 = t3.tail
        val h5 = t4.head
        val t5 = t4.tail
        val h6 = t5.head
        val t6 = t5.tail
        val h7 = t6.head
        
        fn(h7, h6, h5, h4, h3, h2, h1)
      }
    }

}

object HListOps extends HListOps