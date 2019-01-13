package pimpathon

import scala.collection.{mutable ⇒ M}

import pimpathon.mutableMap._
import pimpathon.util._


class MutableMapTest extends PimpathonSuite {
  test("retainKeys") {
    empty.retainKeys(_ ⇒ false)            === empty
    nonEmpty.retainKeys(_ ⇒ false)         === empty
    nonEmpty.retainKeys(_ ⇒ true)          === nonEmpty
    M.Map(1 → 2, 2 → 3).retainKeys(_ == 1) === nonEmpty
  }

  test("retainValues") {
    empty.retainValues(_ ⇒ false)            === empty
    nonEmpty.retainValues(_ ⇒ false)         === empty
    nonEmpty.retainValues(_ ⇒ true)          === nonEmpty
    M.Map(1 → 2, 2 → 3).retainValues(_ == 2) === nonEmpty
  }

  private def empty    = M.Map[Int, Int]()
  private def nonEmpty = M.Map[Int, Int](1 → 2)
}