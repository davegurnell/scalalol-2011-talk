package scalalol
package routes

import net.liftweb.http._

import HListOps._

class Route[Res <: HList](site: Site, path: Path { type Result = Res }, fn: (Res) => LiftResponse) extends RequestHandler {
  
  site.addRoute(this)
  
  def dispatch(req: Req): Option[LiftResponse] =
    path.decode(req.path.partPath).map(fn)
  
}

case class Route0(
  val site: Site,
  val path: Path { type Result = HNil },
  val fn: () => LiftResponse
) extends Route[HNil](site, path, hlistFunction0(fn)) {

  def url() =
    path.encode(HNil).mkString("/", "/", "")

  def apply() =
    fn()

}

case class Route1[A](
  val site: Site,
  val path: Path { type Result = HCons[A, HNil] },
  val fn: (A) => LiftResponse
) extends Route(site, path, hlistFunction1(fn)) {

  def url(a: A) =
    path.encode(HCons(a, HNil)).mkString("/", "/", "")

  def apply(a: A) =
    fn(a)

}

case class Route2[A, B](
  val site: Site,
  val path: Path { type Result = HCons[A, HCons[B, HNil]] },
  val fn: (A, B) => LiftResponse
) extends Route(site, path, hlistFunction2(fn)) {

  def url(a: A, b: B) =
    path.encode(HCons(a, HCons(b, HNil))).mkString("/", "/", "")

  def apply(a: A, b: B) =
    fn(a, b)

}

case class Route3[A, B, C](
  val site: Site,
  val path: Path { type Result = HCons[A, HCons[B, HCons[C, HNil]]] },
  val fn: (A, B, C) => LiftResponse
) extends Route(site, path, hlistFunction3(fn)) {

  def url(a: A, b: B, c: C) =
    path.encode(HCons(a, HCons(b, HCons(c, HNil)))).mkString("/", "/", "")

  def apply(a: A, b: B, c: C) =
    fn(a, b, c)

}

case class Route4[A, B, C, D](
  val site: Site,
  val path: Path { type Result = HCons[A, HCons[B, HCons[C, HCons[D, HNil]]]] },
  val fn: (A, B, C, D) => LiftResponse
) extends Route(site, path, hlistFunction4(fn)) {

  def url(a: A, b: B, c: C, d: D) =
    path.encode(HCons(a, HCons(b, HCons(c, HCons(d, HNil))))).mkString("/", "/", "")

  def apply(a: A, b: B, c: C, d: D) =
    fn(a, b, c, d)

}

case class Route5[A, B, C, D, E](
  val site: Site,
  val path: Path { type Result = HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HNil]]]]] },
  val fn: (A, B, C, D, E) => LiftResponse
) extends Route(site, path, hlistFunction5(fn)) {

  def url(a: A, b: B, c: C, d: D, e: E) =
    path.encode(HCons(a, HCons(b, HCons(c, HCons(d, HCons(e, HNil)))))).mkString("/", "/", "")

  def apply(a: A, b: B, c: C, d: D, e: E) =
    fn(a, b, c, d, e)

}
