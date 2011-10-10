package bigtop
package routes

import java.net.URLEncoder.{encode => urlEncode}
import net.liftweb.http._
import HListOps._

class Route[Res <: HList](site: Site, path: Path { type Result = Res }, fn: (Res) => LiftResponse) extends RequestHandler {
  
  site.addRoute(this)
  
  def dispatch(req: Req): Option[LiftResponse] =
    path.decode(req.path.partPath).map(fn)
  
  def url(arg: Res) =
    path.encode(arg).
         map(urlEncode(_, "utf-8")).
         mkString("/", "/", "")
  
  def apply(arg: Res) =
    fn.apply(arg)
  
}

case class Route0(
  val site: Site,
  val path: Path { type Result = HNil },
  val fn: () => LiftResponse
) extends Route[HNil](site, path, hlistFunction0(fn)) {

  def url(): String =
    url(HNil)

  def apply() =
    fn()

}

case class Route1[A](
  val site: Site,
  val path: Path { type Result = HCons[A, HNil] },
  val fn: (A) => LiftResponse
) extends Route(site, path, hlistFunction1(fn)) {

  def url(a: A): String =
    url(a :: HNil)

  def apply(a: A) =
    fn(a)

}

case class Route2[A, B](
  val site: Site,
  val path: Path { type Result = HCons[A, HCons[B, HNil]] },
  val fn: (A, B) => LiftResponse
) extends Route(site, path, hlistFunction2(fn)) {

  def url(a: A, b: B): String =
    url(a :: b :: HNil)

  def apply(a: A, b: B) =
    fn(a, b)

}

case class Route3[A, B, C](
  val site: Site,
  val path: Path { type Result = HCons[A, HCons[B, HCons[C, HNil]]] },
  val fn: (A, B, C) => LiftResponse
) extends Route(site, path, hlistFunction3(fn)) {

  def url(a: A, b: B, c: C): String =
    url(a :: b :: c :: HNil)

  def apply(a: A, b: B, c: C) =
    fn(a, b, c)

}

case class Route4[A, B, C, D](
  val site: Site,
  val path: Path { type Result = HCons[A, HCons[B, HCons[C, HCons[D, HNil]]]] },
  val fn: (A, B, C, D) => LiftResponse
) extends Route(site, path, hlistFunction4(fn)) {

  def url(a: A, b: B, c: C, d: D): String =
    url(a :: b :: c :: d :: HNil)

  def apply(a: A, b: B, c: C, d: D) =
    fn(a, b, c, d)

}

case class Route5[A, B, C, D, E](
  val site: Site,
  val path: Path { type Result = HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HNil]]]]] },
  val fn: (A, B, C, D, E) => LiftResponse
) extends Route(site, path, hlistFunction5(fn)) {

  def url(a: A, b: B, c: C, d: D, e: E): String =
    url(a :: b :: c :: d :: e :: HNil)

  def apply(a: A, b: B, c: C, d: D, e: E) =
    fn(a, b, c, d, e)

}
