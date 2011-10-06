package bootstrap
package liftweb

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util._

class Boot {

  def boot {
    LiftRules.addToPackages("code")

    LiftRules.dispatch.append(code.Site.dispatchPF)

    LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))    
  }
  
}