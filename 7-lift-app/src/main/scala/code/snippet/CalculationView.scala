package code
package snippet

import scala.xml._
import net.liftweb.util.Helpers._

class CalculationView(calc: Calculation) {
  
  def render =
    ".op *"  #> calc.op &
    ".arg"   #> ("* *" #> calc.args.map(_.toString)) &
    ".ans *" #> calc.ans.toString
  
}
