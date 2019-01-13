package pimpathon

import scala.util.Success

import pimpathon.builder._
import pimpathon.classTag._
import pimpathon.option._
import pimpathon.util._

class OptionTest extends PimpathonSuite {
  test("tap") {
    on(none[String], some("some"))
      .calling(o ⇒ strings().run(ss ⇒ o.tap(ss += "none", ss += _))).produces(List("none"), List("some"))
  }

  test("tapNone") {
    on(none[String], some("some"))
      .calling(o ⇒ strings().run(ss ⇒ o.tapNone(ss += "none"))).produces(List("none"), Nil)
  }

  test("tapSome") {
    on(none[String], some("some"))
      .calling(o ⇒ strings().run(ss ⇒ o.tapSome(ss += _))).produces(Nil, List("some"))
  }

  test("getOrThrow") {
    Some("present").getOrThrow("missing")                === "present"
    Some("present").getOrThrow(new Exception("missing")) === "present"
    Some("present").getOrThrow(util.goBoom: Exception)   === "present"

    util.assertThrows[NoSuchElementException]("missing")(None.getOrThrow("missing"))
    util.assertThrows[RuntimeException]("missing")(None.getOrThrow(new RuntimeException("missing")))
  }

  test("toTry") {
    none[String].toTry.failed.map(_.getClass.getName) === Success[String](className[NoSuchElementException])
    Some("foo").toTry                                 === Success[String]("foo")
  }

  test("invert") { on(none[Int], some(0)).calling(_.invert(1)).produces(some(1), none[Int]) }

  test("amass") {
    on(none[Int], some(1), some(2), some(3))
      .calling(_.amass(util.partial(2 → none, 3 → some("three")))).produces(none, none, none, some("three"))
  }

  test("toEither") {
    none[String].toEither(42, identity[String]) === Left(42)
    some("forty two").toEither(42, identity[String]) === Right("forty two")
    some("change me").toEither(1, s => s.dropRight(2) + "you") === Right("change you")
  }

  test("containedIn") {
    on(some(1), none, some(2)).calling(_.containedIn(Set(0, 1))).produces(true, false, false)
    on(some(1), none, some(2)).calling(_.containedIn(0, 1)).produces(true, false, false)
  }

  private def none[A]: Option[A]       = None
  private def some[A](a: A): Option[A] = Some(a)
}