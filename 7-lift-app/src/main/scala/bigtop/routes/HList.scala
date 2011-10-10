package bigtop
package routes

// HList Implementation based on code by Mark Harrah:
// https://github.com/harrah/up

sealed trait HList

final case class HCons[H, T <: HList](val head : H, val tail : T) extends HList {
  def ::[Next](next: Next) =
    HCons(next, this)
}

sealed abstract class HNil extends HList {
  def ::[X](item: X) =
    HCons(item, this)
}

case object HNil extends HNil
