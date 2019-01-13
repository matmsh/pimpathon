package pimpathon.scalaz

import pimpathon.PimpathonSuite
import pimpathon.util.on
import scalaz.NonEmptyList
import pimpathon.multiMap._
import pimpathon.scalaz.nel._
import pimpathon.util._
import scalaz.syntax.either._

trait Nel

class NelTest extends PimpathonSuite {
  test("unique") {  NonEmptyList(1, 2, 1).unique === NonEmptyList(1, 2) }

  test("uniqueBy") {
    NonEmptyList("foo", "bar", "bard", "food", "foody", "bardo").uniqueBy(_.length) ===
      NonEmptyList("foo", "bard", "foody")
  }

  test("filter") {
    on(NonEmptyList(1), NonEmptyList(1, 2)).calling(_.filter(_ % 2 == 0)).produces(None, Some(NonEmptyList(2)))
  }

  test("filterNot") {
    on(NonEmptyList(2), NonEmptyList(1, 2)).calling(_.filterNot(_ % 2 == 0)).produces(None, Some(NonEmptyList(1)))
  }

  test("max") { NonEmptyList(1, 3, 2).max(scalaz.std.anyVal.intInstance) === 3 }
  test("min") { NonEmptyList(3, 1, 2).min(scalaz.std.anyVal.intInstance) === 1 }

  test("partitionDisjunctions") {
    NonEmptyList(1.left, "abc".right, "def".right, 2.left).partitionDisjunctions[List] ===
      (List(1, 2), List("abc", "def"))
  }

  test("partitionEithers") {
    NonEmptyList[Either[Int, String]](Left(1), Right("abc"), Right("def"), Left(2)).partitionEithers[List] ===
      (List(1, 2), List("abc", "def"))
  }

  test("toMultiMap") {
    on(NonEmptyList((1, 10), (1, 11), (2, 20), (2, 21)))
      .calling(_.toMultiMap[List], _.toMultiMap[Set])
      .produces(Map(1 → List(10, 11), 2 → List(20, 21)), Map(1 → Set(10, 11), 2 → Set(20, 21)))
  }

  test("asMultiMap_withKeys") {
    on(NonEmptyList(0, 1, 2, 3))
      .calling(_.asMultiMap[List].withKeys(_ % 2), _.asMultiMap[NonEmptyList].withKeys(_ % 2))
      .produces(Map(0 → List(0, 2), 1 → List(1, 3)), Map(0 → NonEmptyList(0, 2), 1 → NonEmptyList(1, 3)))
  }

  test("onlyOption") {
    on(NonEmptyList(1), NonEmptyList(1, 2)).calling(_.onlyOption).produces(Some(1), None)
  }

  test("onlyEither") {
    on(NonEmptyList(1, 2), NonEmptyList(1)).calling(_.onlyEither).produces(Left(NonEmptyList(1, 2)), Right(1))
  }

  test("onlyDisjunction") {
    on(NonEmptyList(1, 2), NonEmptyList(1)).calling(_.onlyDisjunction).produces(NonEmptyList(1, 2).left, 1.right)
  }

  test("onlyOrDisjunction") {
    on(NonEmptyList(1, 2), NonEmptyList(1)).calling(_.onlyOrDisjunction(_.size)).produces(2.left, 1.right)
  }
}