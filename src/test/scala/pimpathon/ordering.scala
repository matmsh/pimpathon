package pimpathon

import pimpathon.list.ListPimps
import pimpathon.ordering._
import pimpathon.util._


class OrderingTest extends PimpathonSuite {
  test("promote") {
    on(List(1, 2), List(1, 9), List(9, 2), List(9, 10))
      .calling(_.sorted(Ordering.Int.promote(10, 9))).produces(List(1, 2), List(9, 1), List(9, 2), List(10, 9))
  }

  test("demote") {
    on(List(1, 2), List(1, 9), List(9, 2), List(9, 10))
    .calling(_.sorted(Ordering.Int.demote(2, 1))).produces(List(2, 1), List(9, 1), List(9, 2), List(9, 10))
  }

  test("and") {
    List((1, 2), (1, 3), (2, 3)).shuffle.sorted(Ordering.Int && Ordering.Int) === List((1, 2), (1, 3), (2, 3))
  }

  test("or") {
    List(4, 2, 1, 3).shuffle.sorted(bit(0) || bit(1)) === List(4, 2, 1, 3)
  }

  private def bit(index: Int): Ordering[Int] = Ordering.Boolean.on[Int](i ⇒ (i & (1 << index)) != 0)
}