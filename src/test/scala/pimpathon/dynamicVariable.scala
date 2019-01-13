package pimpathon

import scala.util.DynamicVariable
import dynamicVariable._
import pimpathon.util._


class DynamicVariableTest extends PimpathonSuite {
  test("modify") {
    dyn.modify(_ * 2)
    dyn.value === (123 * 2)
  }

  test("withModification") {
    dyn.withModification(_ * 2) {
      dyn.value === (123 * 2)
      "foo"
    } === "foo"

    dyn.value === 123
  }

  private val dyn = new DynamicVariable[Int](123)
}
