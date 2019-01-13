package pimpathon

import scala.util.{Random, Failure, Success}

import pimpathon.any._
import pimpathon.boolean._
import pimpathon.builder._
import pimpathon.util._


class AnyTest extends  PimpathonSuite {
  test("calc") { List("12".calc(_ + "3"), "12" |> (_ + "3")) === List("123", "123")}
  test("calcIf") { on(2, 3, 4).calling(_.calcIf(_ % 2 == 0)(_ + 3)).produces(Some(5), None, Some(7)) }
  test("calcUnless") { on(2, 3, 4).calling(_.calcUnless(_ % 2 != 0)(_ + 3)).produces(Some(5), None, Some(7)) }

  test("calcPF") {
    on(1, 2, 3, 4).calling(_.calcPF(util.partial(2 → "two", 4 → "four"))).produces(None, Some("two"), None, Some("four"))
  }

  test("transform"){ on(1, 2, 3, 4).calling(_.transform(util.partial(2 → 4, 4 → 8))).produces(1, 4, 3, 8) }

  test("tap") { ints().run(is ⇒ 1.tap(is += _, is += _)) === List(1, 1) }
  test("update") { ints().run(is ⇒ 1.update(is += _, is += _)) === List(1, 1) }
  test("withSideEffect") { ints().run(is ⇒ 1.withSideEffect(is += _, is += _)) === List(1, 1) }

  test("tapIf") { ints().run(is ⇒ List(1, 2, 3).foreach(i ⇒ i.tapIf(_ % 2 == 0)(is += _))) === List(2) }

  test("tapUnless") {
    ints().run(is ⇒ List(1, 2, 3).foreach(i ⇒ i.tapUnless(_ % 2 == 0)(is += _))) === List(1, 3)
  }

  test("tapPF") {
    ints().run(is ⇒ List(1, 2, 3).foreach(i ⇒ i.tapPF { case j if j % 2 != 0 ⇒ is += j })) === List(1, 3)
  }

  test("castTo") {
    on("foo", 123).calling(_.castTo[String]).produces(Some("foo"), None)
  }

  test("cond"){ on("true", "false").calling(_.cond(_ == "true", _ ⇒ "T", _ ⇒ "F")).produces("T", "F")}

  test("partialMatch") {
    on(1, 0).calling(i ⇒ i partialMatch { case 1 ⇒ "Matched" }).produces(Some("Matched"), None)
  }

  test("lpair") { 1.lpair(_ * 10) === (10, 1) }
  test("rpair") { 1.rpair(_ * 10) === (1, 10) }

  test("filterSelf") { on(1, 2, 3, 4).calling(_.filterSelf(_ % 2 == 0)).produces(None, Some(2), None, Some(4)) }
  test("ifSelf") { on(1, 2, 3, 4).calling(_.ifSelf(_ % 2 == 0)).produces(None, Some(2), None, Some(4)) }

  test("filterNotSelf") {
    on(1, 2, 3, 4).calling(_.filterNotSelf(_ % 2 == 0)).produces(Some(1), None, Some(3), None)
  }

  test("unlessSelf") { on(1, 2, 3, 4).calling(_.unlessSelf(_ % 2 == 0)).produces(Some(1), None, Some(3), None) }

  test("containedIn") { on(1, 2, 3, 4).calling(_.containedIn(Set(1, 3))).produces(true, false, true, false) }
  test("notContainedIn") { on(1, 2, 3, 4).calling(_.notContainedIn(Set(1, 3))).produces(false, true, false, true) }

  test("isOneOf") { on(1, 2, 3, 4).calling(_.isOneOf(1, 3)).produces(true, false, true, false) }
  test("isNotOneOf"){ on(1, 2, 3, 4).calling(_.isNotOneOf(1, 3)).produces(false, true, false, true) }

  test("passes_one") {
    on(1, 2, 3, 4).calling(_.passes.one(_ < 2, _ > 3)).produces(Some(1), None, None, Some(4))
    on(1, 2, 3, 4).calling(_.passes.one()).produces(None, None, None, None)
  }

  test("passes_all") {
    on(1, 2, 3, 4).calling(_.passes.all(_ >= 2, _ <= 3)).produces(None, Some(2), Some(3), None)
    on(1, 2, 3, 4).calling(_.passes.all()).produces(Some(1), Some(2), Some(3), Some(4))
  }

  test("passes_none()"){
    on(1, 2, 3, 4).calling(_.passes.none(_ >= 2, _ <= 3)).produces(Some(1), None, None, Some(4))
    on(1, 2, 3, 4).calling(_.passes.none()).produces(None, None, None, None)
  }

  test("passes_some()") {
    on(1, 2, 3, 4).calling(_.passes.some(_ < 2, _ > 3)).produces(None, Some(2), Some(3), None)
    on(1, 2, 3, 4).calling(_.passes.some()).produces(Some(1), Some(2), Some(3), Some(4))
  }

  test("fails_one") {
    on(1, 2, 3, 4).calling(_.fails.one(_ < 2, _ > 3)).produces(None, Some(2), Some(3), None)
    on(1, 2, 3, 4).calling(_.fails.one()).produces(Some(1), Some(2), Some(3), Some(4))
  }

  test("fails_all") {
    on(1, 2, 3, 4).calling(_.fails.all(_ >= 2, _ <= 3)).produces(Some(1), None, None, Some(4))
    on(1, 2, 3, 4).calling(_.fails.all()).produces(None, None, None, None)
  }

  test("fails_none") {
    on(1, 2, 3, 4).calling(_.fails.none(_ >= 2, _ <= 3)).produces(None, Some(2), Some(3), None)
    on(1, 2, 3, 4).calling(_.fails.none()).produces(Some(1), Some(2), Some(3), Some(4))
  }

  test("fails_some") {
    on(1, 2, 3, 4).calling(_.fails.some(_ < 2, _ > 3)).produces(Some(1), None, None, Some(4))
    on(1, 2, 3, 4).calling(_.fails.some()).produces(None, None, None, None)
  }

  test("withFinally") {
    strings().run(ss ⇒ {
      ss += "input".withFinally(s ⇒ ss += "finally: " + s)(s ⇒ {
        ss += "body: " + s; "done"
      })
    }) === List("body: input", "finally: input", "done")
  }

  test("tryFinally") {
    strings().run(ss ⇒ {
      ss += "input".tryFinally(s ⇒ {
        ss += "body: " + s; "done"
      })(s ⇒ ss += "finally: " + s)
    }) === List("body: input", "finally: input", "done")
  }

  test("attempt") { List(1.attempt(_ * 2), 1.attempt(_ ⇒ throw boom)) === List(Success(2), Failure(boom)) }

  test("addTo") { ints().run(is ⇒ 1.addTo(is)) === List(1) }
  test("removeFrom") { ints(1).tap(is ⇒ 1.removeFrom(is)).toList === Nil }

  test("unfold") { 64.unfold(i ⇒ (i > 1).option((i, i/2))).toList === List(64, 32, 16, 8, 4, 2) }

  test("bounded") {
    Stream.fill(10)(Random.nextInt()).foreach(num ⇒ num.bounded(10, 100) === ((10 max num) min 100))

    Stream.fill(10)(Random.nextDouble()).foreach(num ⇒ {
      val dd: Double =(10.0 max num) min 100.0

      val actual =(10.0 max num) min 100.0
      val expected = num.bounded(10.0, 100.0)
      actual === expected
      //assert((10.0 max num) min 100.0 === num.bounded(10.0, 100.0) +- 0.01)
    })
  }
}