package scalalol
package routes

// HList Implementation based on code by Mark Harrah:
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

sealed abstract class HNil extends HList {
  type Head = Nothing
  type Tail = HNil
  
  def ::[Next](next: Next) =
    HCons(next, this)
}

case object HNil extends HNil
