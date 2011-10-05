trait RouteBuilder {
  
  trait PimpedPath[Rt <: Route[_]] {
    type Fn
    def >>(fn: Fn)(implicit site: Site): Rt
  }
  
  implicit def promotePath0(path: Path { type Result = HNil }) =
    new PimpedPath[Route0] {
      type Fn = () => Response
      
      def >>(fn: Fn)(implicit site: Site) = Route0(path, fn)(site)
    }
  
  implicit def promotePath1[A](path: Path { type Result = HCons[A, HNil] }) =
    new PimpedPath[Route1[A]] {
      type Fn = (A) => Response
      
      def >>(fn: Fn)(implicit site: Site) = Route1(path, fn)(site)
    }
  
  implicit def promotePath2[A, B](path: Path { type Result = HCons[B, HCons[A, HNil]] }) =
    new PimpedPath[Route2[A, B]] {
      type Fn = (A, B) => Response

      def >>(fn: Fn)(implicit site: Site) = Route2(path, fn)(site)
    }
  
  implicit def promotePath3[A, B, C](path: Path { type Result = HCons[C, HCons[B, HCons[A, HNil]]] }) =
    new PimpedPath[Route3[A, B, C]] {
      type Fn = (A, B, C) => Response

      def >>(fn: Fn)(implicit site: Site) = Route3(path, fn)(site)
    }
  
  implicit def promotePath4[A, B, C, D](path: Path { type Result = HCons[D, HCons[C, HCons[B, HCons[A, HNil]]]] }) =
    new PimpedPath[Route4[A, B, C, D]] {
      type Fn = (A, B, C, D) => Response

      def >>(fn: Fn)(implicit site: Site) = Route4(path, fn)(site)
    }
  
  implicit def promotePath5[A, B, C, D, E](path: Path { type Result = HCons[E, HCons[D, HCons[C, HCons[B, HCons[A, HNil]]]]] }) =
    new PimpedPath[Route5[A, B, C, D, E]] {
      type Fn = (A, B, C, D, E) => Response

      def >>(fn: Fn)(implicit site: Site) = Route5(path, fn)(site)
    }

}