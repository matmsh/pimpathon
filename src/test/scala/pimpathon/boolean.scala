package pimpathon

import pimpathon.boolean._
import pimpathon.scalaz.std.boolean._
import _root_.scalaz.{-\/, \/-}


class BooleanTest extends PimpathonSuite {
  test("asInt") { falseTrue(_.asInt).produces(0, 1) }
  test("either_or") {  falseTrue(_.either(123).or("456")).produces(Left("456"), Right(123))}
  test("option") { falseTrue(_.option(123)).produces(None, Some(123)) }
  test("cond") {  falseTrue(_.cond(123, 456)).produces(456, 123) }
  test("implies") { truthTableFor(_ implies _, t, t, f, t) }
  test("nor") { truthTableFor(_ nor _,     t, f, f, f) }
  test("nand") { truthTableFor(_ nand _,    t, t, t, f) }

  test("disjunction_or") { falseTrue(_.disjunction(123).or("456")).produces(-\/("456"), \/-(123)) }

  private def truthTableFor(fn: (Boolean, Boolean) => Boolean, ff: Boolean, ft: Boolean, tf: Boolean, tt: Boolean): Unit =
    util.on((f,f), (f,t), (t,f), (t,t)).calling(fn.tupled).produces(ff, ft, tf, tt)

  private def falseTrue[A](f: Boolean ⇒ A) = util.on(false, true).calling(f)
  private val (t, f) = (true, false)
}