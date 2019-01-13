package pimpathon.frills

import pimpathon.PimpathonSuite
import pimpathon.frills.any._
import pimpathon.util._
import scalaz.syntax.validation._


class AnyTest extends PimpathonSuite {
  test("ensure")  { on(1, 2).calling(_.ensure("odd")(isEven)).produces("odd".failure, 2.success) }
  
  test("ensureNel") { on(1, 2).calling(_.ensureNel("odd")(isEven)).produces("odd".failureNel, 2.successNel) }

  private val isEven = (i: Int) ⇒ i % 2 == 0
}