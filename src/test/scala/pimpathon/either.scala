package pimpathon

import scala.util.{Failure, Success}

import pimpathon.builder._
import pimpathon.either.EitherPimps
import pimpathon.function._
import pimpathon.tuple._
import pimpathon.util._


class EitherTest extends PimpathonSuite {
  test("leftOr") { on(Left("left"), Right("right")).calling(_.leftOr(_ + " !")).produces("left", "right !") }
  test("rightOr") { on(Left("left"), Right("right")).calling(_.rightOr(_ + " !")).produces("left !", "right") }

  test("rescue") { on(Right(123), Left("456")).calling(_.rescue(_.toInt)).produces(123, 456)}
  test("valueOr"){ on(Right(123), Left("456")).calling(_.valueOr(_.toInt)).produces(123, 456) }

  test("rescuePF") {
    on(Right(123), Left("456"), Left("123"))
      .calling(_.rescue(util.partial("123" → 123))).produces(Right(123), Left("456"), Right(123))
  }

  test("valueOrPF") {
    on(Right(123), Left("456"), Left("123"))
      .calling(_.valueOr(util.partial("123" → 123))).produces(Right(123), Left("456"), Right(123))
  }

  test("bimap") {
    on(left(1), right("foo"))
      .calling(_.bimap(_.toString, _.length)).produces(Left[String, Int]("1"), Right[String, Int](3))
  }

  test("leftMap") {
    on(left(1), right("foo"))
      .calling(_.leftMap(_.toString)).produces(Left[String, String]("1"), Right[String, String]("foo"))
  }

  test("rightMap") {
    on(left(1), right("foo"))
      .calling(_.rightMap(_.length)).produces(Left[Int, Int](1), Right[Int, Int](3))
  }


  test("leftFlatMap") {
    on(Right(123), Left("456"), Left("123"))
      .calling(_.leftFlatMap(partial("123" → 123).toRight)).produces(Right(123), Left("456"), Right(123))
  }

  test("rightFlatMap") {
    on(Left(123), Right("456"), Right("123"))
      .calling(_.rightFlatMap(partial("123" → 123).toLeft)).produces(Left(123), Right("456"), Left(123))
  }

  test("tap()") {
    (ints(), strings()).tap(is ⇒ ss ⇒ left(1).tap(is += _, ss += _)).tmap(_.reset(), _.reset()) === (List(1), Nil)

    (ints(), strings()).tap(is ⇒ ss ⇒ right("foo").tap(is += _, ss += _)).tmap(_.reset(), _.reset()) === (
      Nil, List("foo")
    )
  }

  test("tapLeft") {
    ints().run(is ⇒      left(1).tapLeft(is += _)) === List(1)
    ints().run(is ⇒ right("foo").tapLeft(is += _)) === Nil
  }

  test("tapRight") {
    strings().run(ss ⇒      left(1).tapRight(ss += _)) === Nil
    strings().run(ss ⇒ right("foo").tapRight(ss += _)) === List("foo")
  }

  test("addTo") {
    (ints(), strings()).tap(is ⇒ ss ⇒ left(1).addTo(is, ss)).tmap(_.result(), _.result())      === (List(1), Nil)
    (ints(), strings()).tap(is ⇒ ss ⇒ right("foo").addTo(is, ss)).tmap(_.result(), _.result()) === (Nil, List("foo"))
  }

  test("removeFrom") {
    (ints(1), strings("oo")).tap(is ⇒ ss ⇒ left(1).removeFrom(is, ss)).tmap(_.toList, _.toList) === (Nil, List("oo"))
    (ints(1), strings("oo")).tap(is ⇒ ss ⇒ right("oo").removeFrom(is, ss)).tmap(_.toList, _.toList) === (List(1), Nil)
  }

  test("getMessage") {
    on(Left(boom), Right("foo")).calling(_.getMessage).produces(Some(boom.getMessage), None)
  }

  test("toTry") {
    on(Left[Throwable, String](boom), Right[Throwable, String]("foo"))
      .calling(_.toTry).produces(Failure[String](boom), Success[String]("foo"))
  }

  test("toOption") {
    on(Left[Throwable, String](boom), Right[Throwable, String]("foo"))
      .calling(_.toOption).produces(None, Some("foo"))
  }

  private def left(i: Int): Either[Int, String] = Left(i)
  private def right(s: String): Either[Int, String] = Right(s)
}