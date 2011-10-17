package bigtop
package routes

import net.liftweb.http._

trait RouteBuilder {
  
  trait PimpedPath[Rt <: Route[_]] {
    type Fn
    def >>(fn: Fn)(implicit site: Site): Rt
  }
  
  implicit def pimpPath0(path: Path { type Result = HNil }) =
    new PimpedPath[Route0] {
      type Fn = () => LiftResponse
      
      def >>(fn: Fn)(implicit site: Site) =
        Route0(site, path, fn)
    }
  
  implicit def pimpPath1[A](path: Path { type Result = HCons[A, HNil] }) =
    new PimpedPath[Route1[A]] {
      type Fn = (A) => LiftResponse
      
      def >>(fn: Fn)(implicit site: Site) =
        Route1(site, path, fn)
    }
  
  implicit def pimpPath2[A, B](path: Path { type Result = HCons[A, HCons[B, HNil]] }) =
    new PimpedPath[Route2[A, B]] {
      type Fn = (A, B) => LiftResponse

      def >>(fn: Fn)(implicit site: Site) =
        Route2(site, path, fn)
    }
  
  implicit def pimpPath3[A, B, C](path: Path { type Result = HCons[A, HCons[B, HCons[C, HNil]]] }) =
    new PimpedPath[Route3[A, B, C]] {
      type Fn = (A, B, C) => LiftResponse

      def >>(fn: Fn)(implicit site: Site) =
        Route3(site, path, fn)
    }
  
  implicit def pimpPath4[A, B, C, D](path: Path { type Result = HCons[A, HCons[B, HCons[C, HCons[D, HNil]]]] }) =
    new PimpedPath[Route4[A, B, C, D]] {
      type Fn = (A, B, C, D) => LiftResponse

      def >>(fn: Fn)(implicit site: Site) =
        Route4(site, path, fn)
    }
  
  implicit def pimpPath5[A, B, C, D, E](path: Path { type Result = HCons[A, HCons[B, HCons[C, HCons[D, HCons[E, HNil]]]]] }) =
    new PimpedPath[Route5[A, B, C, D, E]] {
      type Fn = (A, B, C, D, E) => LiftResponse

      def >>(fn: Fn)(implicit site: Site) =
        Route5(site, path, fn)
    }

}

object RouteBuilder extends RouteBuilder
