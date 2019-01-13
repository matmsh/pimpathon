package pimpathon

import scala.util.{Failure, Success, Random}

import pimpathon.any._
import pimpathon.function._
import pimpathon.util._


class FunctionTest extends PimpathonSuite {
  test("attempt"){
    ((i: Int) ⇒ i).attempt(3)      === Success(3)
    ((i: Int) ⇒ goBoom).attempt(3) === Failure(boom)
  }

  test("guardWith") {
    on(1, 2, 3, 4).calling((double guardWith isEven).lift).produces(None, Some(4), None, Some(8))
  }

  test("unlift") {
    f.unlift.calc(pf ⇒ {
      on(0, 1, 2, 3).calling(pf.isDefinedAt).produces(true, false, true, false)
      on(0, 2).calling(pf.apply).produces(0, 2)
      assertThrows[MatchError](pf(1))
      pf.lift === f
    })
  }

  test("tuple2") {  f.tuple2.apply(1, 2)         === (None, Some(2)) }
  test("tuple3") { f.tuple3.apply(1, 2, 3)       === (None, Some(2), None) }
  test("tuple4") { f.tuple4.apply(1, 2, 3, 4)    === (None, Some(2), None, Some(4)) }
  test("tuple5") { f.tuple5.apply(1, 2, 3, 4, 5) === (None, Some(2), None, Some(4), None) }

  private val f: (Int) ⇒ Option[Int] = (i: Int) ⇒ i.filterSelf(_ % 2 == 0)
  private val isEven: Predicate[Int] = _ % 2 == 0
  private val double: (Int ⇒ Int)   = _ * 2
}

class Function2Test extends PimpathonSuite {
  test("tuple2") { f.tuple2.apply((1, 2),          (10, 20))             === (11, 22) }
  test("tuple3") { f.tuple3.apply((1, 2, 3),       (10, 20, 30))         === (11, 22, 33) }
  test("tuple4") { f.tuple4.apply((1, 2, 3, 4),    (10, 20, 30, 40))     === (11, 22, 33, 44) }
  test("tuple5") { f.tuple5.apply((1, 2, 3, 4, 5), (10, 20, 30, 40, 50)) === (11, 22, 33, 44, 55) }

  private val f: (Int, Int) ⇒ Int = _ + _
}

class CurriedFunction2Test extends PimpathonSuite {
  test("tupled") {  ((i: Int) ⇒ (j: Int) ⇒ i + j).tupled((1, 2)) === 3 }
}

class PredicateTest extends PimpathonSuite {
  test("cond") {  List(2, 3, 4, 6).map(isEven.cond("even", "odd")) === List("even", "odd", "even", "even") }

  test("and") {
    List(2, 3, 4, 6).filter(isEven and (_ > 2))          === List(4, 6)
    List(2, 3, 4, 6).filter(function.and(isEven, _ > 2)) === List(4, 6)
    assert(function.and[Int]().apply(Random.nextInt()))
  }

  test("or")  {
    List(2, 1, 4, 3, 5).filter(isEven or (_ == 3))          === List(2, 4, 3)
    List(2, 1, 4, 3, 5).filter(function.or(isEven, _ == 3)) === List(2, 4, 3)
    assert(!function.or[Int]().apply(Random.nextInt()))
  }

  test("not") { List(2, 1, 4, 3, 5).filter(isEven.not) === List(1, 3, 5) }

  test("exists") {
    List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.exists) ===
      List(List(2), List(2, 4), List(2, 4, 3))
  }

  test("forall") {
    List(Nil, List(2), List(3), List(2, 4), List(2, 4, 3)).filter(isEven.forall) === List(Nil, List(2), List(2, 4))
  }

  test("ifSome") {
    List(None, Some(3), Some(4), None, Some(6)).filter(isEven.ifSome) ===
      List(Some(4), Some(6))
  }

  test("first")  { List((1, 2), (2, 3)).filter(isEven.first[Int])  === List((2, 3)) }
  test("second") { List((1, 2), (2, 3)).filter(isEven.second[Int]) === List((1, 2)) }

  test("guard") { on(1, 2, 3, 4).calling((isEven guard double).lift).produces(None, Some(4), None, Some(8)) }

  test("nand") {
    List(2, 3, 4, 6).filter(function.nand(isEven, _ > 2)) === List(2, 3)
    assert(!function.nand[Int]().apply(Random.nextInt()))
  }

  test("nor") {
    List(2, 1, 4, 3, 5).filter(function.nor(isEven, _ == 3)) === List(1, 5)
    assert(function.nor[Int]().apply(Random.nextInt()))
  }

  private val isEven: Predicate[Int] = _ % 2 == 0
  private val double: Int ⇒ Int   = _ * 2
}

class PartialFunctionTest extends PimpathonSuite {
  test("either")  {  on(1, 2).calling(util.partial(1 → "2").either).produces(Right("2"), Left(2)) }
  test("toRight") {  on(1, 2).calling(util.partial(1 → "2").toRight).produces(Right("2"), Left(2)) }
  test("toLeft")  {  on(1, 2).calling(util.partial(1 → "2").toLeft).produces(Left("2"), Right(2)) }
  test("unify")   {  on(1, 2).calling(util.partial(2 → 4).unify).produces(1, 4) }

  test("isUndefinedAt") {
    on("oof", "foo").calling(util.partial("foo" → "bar").isUndefinedAt).produces(true, false)
  }

  test("first") {
    on(1 → "foo", 2 → "bar").calling(util.partial(1 → 2).first[String].lift).produces(Some(2 → "foo"), None)
  }

  test("second") {
    on("foo" → 1, "bar" → 2).calling(util.partial(1 → 2).second[String].lift).produces(Some("foo" → 2), None)
  }

  test("partition") {
    util.partial(1 → "one", 3 → "three").partition[List](List(1, 2, 3, 4)) === ((List(2, 4), List("one", "three")))
  }

  test("starStarStar") {
    on((1, 2), (0, 2), (1, 0))
      .calling((util.partial(1 → 2) *** util.partial(2 → 3)).lift).produces(Some((2, 3)), None, None)
  }

  test("ampAmpAmp") {
    on(1, 2, 3)
      .calling((util.partial(1 → 2, 2 → 3) &&& util.partial(1 → 3, 3 → 4)).lift).produces(Some((2, 3)), None, None)
  }

  test("pipePipePipe") {
    on(Left(1), Right("two"), Left(3), Right("four"))
      .calling((util.partial(1 → 11) ||| util.partial("two" → 22)).lift).produces(Some(11), Some(22), None, None)
  }

  test("map") {
    on(1, 2, 3)
      .calling(util.partial(1 → 2, 2 → 3).map(_.toString).lift).produces(Some("2"), Some("3"), None)
  }

  test("contramap") {
    on(10, 20, 30)
      .calling(util.partial(1 → 2, 2 → 3).contramap[Int](_ / 10).lift).produces(Some(2), Some(3), None)
  }

  test("partialChain") {
    def reverse(prefix: Int)(value: String): String = prefix + value.reverse
    def duplicate(prefix: Int)(value: String): String = prefix + value + value

    function.partialChain(reverse, duplicate)(1)("Bolton") === "11notloB1notloB"
  }

  test("partialChain2") {
    def reverse(prefix: Int, suffix: String)(value: String): String = prefix + value.reverse + suffix
    def duplicate(prefix: Int, suffix: String)(value: String): String = prefix + value + value + suffix

    function.partialChain2(reverse, duplicate)(1, "2")("Bolton") === "11notloB21notloB22"
  }
}