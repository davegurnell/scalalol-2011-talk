import HListOps._

class Route[Res <: HList](path: Path { type Result = Res }, fn: (Res) => Response)(val site: Site) {
  
  site.addRoute(this)
  
  def dispatch(req: Request): Option[Response] =
    path.decode(req.path).map(fn)
  
  // def url(arg: Res) =
  //   path.encode(arg).mkString("/", "/", "")
  // 
  // def apply(arg: Res) =
  //   fn.apply(arg)
  
}

case class Route0(
  val path: Path { type Result = HNil },
  val fn: () => Response
)(implicit site: Site) extends Route[HNil](
  path, 
  hlistFunction0(fn)
)(site) {
  
  def url() =
    path.encode(HNil).mkString("/", "/", "")
  
  def apply() =
    fn()
  
}

case class Route1[A](
  val path: Path { type Result = HCons[A, HNil] },
  val fn: (A) => Response
)(implicit site: Site) extends Route[HCons[A, HNil]](
  path,
  hlistFunction1(fn)
)(site) {
  
  def url(a: A) = path.encode(HCons(a, HNil)).mkString("/", "/", "")
  
  def apply(a: A) = fn(a)
  
}

case class Route2[A, B](
  val path: Path { type Result = HCons[B, HCons[A, HNil]] },
  val fn: (A, B) => Response
)(implicit site: Site) extends Route[HCons[B, HCons[A, HNil]]](
  path,
  hlistFunction2(fn)
)(site) {
  def url(a: A, b: B) = path.encode(HCons(b, HCons(a, HNil))).mkString("/", "/", "")
  def apply(a: A, b: B) = fn(a, b)
}

case class Route3[A, B, C](
  val path: Path { type Result = HCons[C, HCons[B, HCons[A, HNil]]] },
  val fn: (A, B, C) => Response
)(implicit site: Site) extends Route[HCons[C, HCons[B, HCons[A, HNil]]]](
  path,
  hlistFunction3(fn)
)(site) {
  def url(a: A, b: B, c: C) = path.encode(HCons(c, HCons(b, HCons(a, HNil)))).mkString("/", "/", "")
  def apply(a: A, b: B, c: C) = fn(a, b, c)
}

case class Route4[A, B, C, D](
  val path: Path { type Result = HCons[D, HCons[C, HCons[B, HCons[A, HNil]]]] },
  val fn: (A, B, C, D) => Response
)(implicit site: Site) extends Route[HCons[D, HCons[C, HCons[B, HCons[A, HNil]]]]](
  path,
  hlistFunction4(fn)
)(site) {
  def url(a: A, b: B, c: C, d: D) = path.encode(HCons(d, HCons(c, HCons(b, HCons(a, HNil))))).mkString("/", "/", "")
  def apply(a: A, b: B, c: C, d: D) = fn(a, b, c, d)
}

case class Route5[A, B, C, D, E](
  val path: Path { type Result = HCons[E, HCons[D, HCons[C, HCons[B, HCons[A, HNil]]]]] },
  val fn: (A, B, C, D, E) => Response
)(implicit site: Site) extends Route[HCons[E, HCons[D, HCons[C, HCons[B, HCons[A, HNil]]]]]](
  path,
  hlistFunction5(fn)
)(site) {
  def url(a: A, b: B, c: C, d: D, e: E) = path.encode(HCons(e, HCons(d, HCons(c, HCons(b, HCons(a, HNil)))))).mkString("/", "/", "")
  def apply(a: A, b: B, c: C, d: D, e: E) = fn(a, b, c, d, e)
}
