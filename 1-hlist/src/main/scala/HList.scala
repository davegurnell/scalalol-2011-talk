// ----------------------------------------------
// HList Implementation based on Mark Harrah's Up
// https://github.com/harrah/up

sealed trait HList {
  type Head
  type Tail <: HList
}

final case class HCons[H, T <: HList](val head : H, val tail : T) extends HList {
  type Head = H
  type Tail = T
  
  def ::[Next](next: Next) =
    HCons(next, this)
}

sealed class HNil extends HList {
  type Head = Nothing
  type Tail = HNil
  
  def ::[Next](next: Next) =
    HCons(next, this)
}

case object HNil extends HNil

/*

Try these:

val x0 = Nil
val y0 = HNil

val x1 = 1 :: Nil
val y1 = 1 :: HNil

val x2 = 2 :: 1 :: Nil
val y2 = 2 :: 1 :: HNil

val x3 = 2.0 :: 1 :: Nil
val y3 = 2.0 :: 1 :: HNil

*/