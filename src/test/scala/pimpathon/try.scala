package pimpathon

import scala.util.{Failure, Success}

import pimpathon.pimpTry._
import pimpathon.util._


class TryTest extends  PimpathonSuite {
  test("fold") {
    on(Success("foo"), Failure(boom)).calling(_.fold(_.getMessage, s ⇒ s)).produces("foo", boom.getMessage)
  }

  test("getMessage") {
    on(Success("foo"), Failure(boom)).calling(_.getMessage).produces(None, Some(boom.getMessage))
  }

  test("toEither") {
    on(Success("foo"), Failure(boom)).calling(_.toEither).produces(Right("foo"), Left(boom))
  }
}