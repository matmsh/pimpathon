package pimpathon

import pimpathon.function.Predicate
import scala.collection.{mutable ⇒ M}

import pimpathon.any._
import pimpathon.stream._
import pimpathon.util._


class StreamTest extends PimpathonSuite {
  test("cond") {
    stream.cond[Int](cond = false, util.goBoom) === Stream.empty[Int]
    stream.cond(cond = true, Stream(1, 2, 3))   === Stream(1, 2, 3)
  }

  test("continuallyWhile") {
    stream.continuallyWhile(1)(_ ⇒ false)                  === Stream.empty[Int]
    stream.continuallyWhile(1)(_ ⇒ true).take(1000).toList === List.fill(1000)(1)
    M.Stack[Int](1, 2, 3).tap(ints ⇒ stream.continuallyWhile(ints.pop())(_ < 3).toList === List(1, 2))
  }

  test("uncons") {
    on(Stream.empty[Int], Stream(1, 2, 3))
      .calling(_.uncons("empty", s ⇒ s"size: ${s.size}")).produces("empty", "size: 3")
  }

  test("unconsC") {
    on(Stream.empty[Int], Stream(1, 2, 3))
      .calling(_.unconsC("empty", h ⇒ t ⇒ s"size: ${1 + t.size}")).produces("empty", "size: 3")
  }

  test("tailOption") {
    on(Stream.empty[Int], Stream(0), Stream(0, 1)).calling(_.tailOption)
      .produces(None, Some(Stream.empty[Int]), Some(Stream(1)))
  }

  test("lazyScanLeft") {
    blockingInts(start = 1, end = 4).lazyScanLeft(0)(_ + _).take(4).toList === List(0, 1, 3, 6)
  }

  test("reverseInits()") {
    blockingInts(start = 1, end = 4).reverseInits.take(4).toList.map(_.toList) ===
      List(Nil, List(1), List(1, 2), List(1, 2, 3))
  }

  private def blockingInts(start: Int, end: Int): Stream[Int] = blockWhen(Stream.iterate(start)(_ + 1))(_ == end)
  private def blockWhen[A](in: Stream[A])(p: Predicate[A]): Stream[A] = in.map(_.tapIf(p)(_ ⇒ block()))
  private def block(): Unit = this.synchronized(this.wait(0))
}