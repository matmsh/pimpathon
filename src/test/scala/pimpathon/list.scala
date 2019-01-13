package pimpathon

import pimpathon.builder._
import pimpathon.list._
import pimpathon.util._
import pimpathon.either._


class ListTest extends PimpathonSuite {
  test("uncons") { on(nil[Int], List(1, 2, 3))
    .calling(_.uncons("empty", l ⇒ s"size: ${l.size}")).produces("empty", "size: 3")
  }

  test("unconsC") {
    on(nil[Int], List(1, 2, 3))
      .calling(_.unconsC("empty", h ⇒ t ⇒ s"head: $h, tail: $t")).produces("empty", "head: 1, tail: List(2, 3)")
  }

  test("unsnocC") {
    on(nil[Int], List(1, 2, 3))
      .calling(_.unsnocC("empty", i ⇒ l ⇒ s"init: $i, last: $l")).produces("empty", "init: List(1, 2), last: 3")
  }

  test("emptyTo") { on(nil[Int], List(1, 2, 3)).calling(_.emptyTo(List(1))).produces(List(1), List(1, 2, 3)) }

  test("calcIfNonEmpty") { on(nil[Int], List(1, 2, 3))
    .calling(_.calcIfNonEmpty(_.reverse)).produces(None, Some(List(3, 2, 1)))
  }

  test("mapIfNonEmpty") { on(nil[Int], List(1, 2, 3))
    .calling(_.mapIfNonEmpty(_ * 2)).produces(None, Some(List(2, 4, 6)))
  }

  test("zipToMap") {
    nil[Int].zipToMap(nil[Int]) === Map.empty[Int, Int]
    List(1).zipToMap(List(2))   === Map(1 → 2)
  }

  test("zipWith") { List(2, 0).zipWith(List(3))(lr ⇒ lr._1 * lr._2) === List(6) }

  test("countWithSize") {
    on(nil[Int], List(0), List(1), List(0, 1))
      .calling(_.countWithSize(_ < 1)).produces(None, Some((1, 1)), Some((0, 1)), Some((1, 2)))
  }

  test("sizeGT") {
    assert(nil[Int].sizeGT(-1))
    assert(!nil[Int].sizeGT(0))
    assert(List(1, 2).sizeGT(1))
    assert(!List(1, 2).sizeGT(2))
  }

  test("duplicates") {
    List("foo", "bar", "foo", "food", "bar", "foo").duplicates === List("bar", "bar", "foo", "foo", "foo")
  }

  test("duplicatesBy") {
    List("foo", "bar", "bard", "food", "foody").duplicatesBy(_.length) === List("bard", "food", "foo", "bar")
  }

  test("distinctBy") {
    List("foo", "bar", "bard", "food", "foody", "bardo").distinctBy(_.length) === List("foo", "bard", "foody")
  }

  test("countBy") { List("foo", "bard", "food", "barb", "foody", "barby").countBy(_.length) ===
    Map(1 → List("foo"), 2 → List("foody", "barby"), 3 → List("bard", "food", "barb"))
  }

  test("tailOption") {
    on(nil[Int], List(0), List(0, 1)).calling(_.tailOption).produces(None, Some(Nil), Some(List(1)))
  }

  test("headTail") {
    on(List(1), List(1, 2), List(1, 2, 3)).calling(_.headTail).produces((1, Nil), (1, List(2)), (1, List(2, 3)))
    util.assertThrows[NoSuchElementException]("headTail of empty list")(Nil.headTail)
  }

  test("initLast") {
    on(List(1), List(1, 2), List(1, 2, 3)).calling(_.initLast).produces((Nil, 1), (List(1), 2), (List(1, 2), 3))
    util.assertThrows[NoSuchElementException]("initLast of empty list")(Nil.initLast)
  }

  test("headTailOption") {
    on(nil[Int], List(1), List(1, 2), List(1, 2, 3))
      .calling(_.headTailOption).produces(None, Some((1, Nil)), Some((1, List(2))), Some((1, List(2, 3))))
  }

  test("initLastOption") {
    on(nil[Int], List(1), List(1, 2), List(1, 2, 3))
      .calling(_.initLastOption).produces(None, Some((Nil, 1)), Some((List(1), 2)), Some((List(1, 2), 3)))
  }

  test("initOption") {
    on(nil[Int], List(1), List(1, 2), List(1, 2, 3))
      .calling(_.initOption).produces(None, Some(Nil), Some(List(1)), Some(List(1, 2)))
  }

  test("const") { on(nil[Int], List('a', 'b', 'c')).calling(_.const(1)).produces(nil[Int], List(1, 1, 1)) }

  test("mapC") {
    on(nil[(Int, Int)], List((1, 2), (2, 3))).calling(_.mapC(a ⇒ b ⇒ a * b)).produces(nil[Int], List(2, 6))
  }

  test("mapFirst") {
    on(nil[(Int, Int)], List((1, 2), (2, 3))).calling(_.mapFirst(_ * 2)).produces(nil[(Int, Int)], List((2, 2), (4, 3)))
  }

  test("mapSecond") {
    on(nil[(Int, Int)], List((1, 2), (2, 3))).calling(_.mapSecond(_ * 2)).produces(nil[(Int, Int)], List((1, 4), (2, 6)))
  }

  test("mapValues") {
    on(nil[(Int, Int)], List((1, 2), (2, 3))).calling(_.mapValues(_ * 2)).produces(nil[(Int, Int)], List((1, 4), (2, 6)))
  }

  test("flatMapValues") {
    on(nil[(String, Int)], List(("a", 1), ("b", 2)))
      .calling(_.flatMapValues(v => (0 to 2).map(i => v * i)))
      .produces(nil[(String, Int)], List(("a", 0), ("a", 1), ("a", 2), ("b", 0), ("b", 2), ("b", 4)))
    on(nil[(String, Int)], List(("a", 1), ("b", 2)))
      .calling(_.flatMapValues{ case 1 => Some(10); case _ => None })
      .produces(nil[(String, Int)], List(("a", 10)))
  }

  test("keys") {
    List((2, 1), (4, 2), (6, 3), (4, 1)).keys === List(2, 4, 6, 4)
  }

  test("values") {
    List((2, 1), (4, 2), (6, 3), (4, 1)).values === List(1, 2, 3, 1)
  }

  test("lpair") {
    on(nil[Int], List(1, 2, 3)).calling(_.lpair(_ * 2)).produces(nil[(Int, Int)], List((2, 1), (4, 2), (6, 3)))
  }

  test("rpair") {
    on(nil[Int], List(1, 2, 3)).calling(_.rpair(_ * 2)).produces(nil[(Int, Int)], List((1, 2), (2, 4), (3, 6)))
  }

  test("sharedPrefix") {
    nil[Int].sharedPrefix(Nil)                      === (Nil, Nil, Nil)
    List(1).sharedPrefix(List(1))                   === (List(1), Nil, Nil)
    List(1, 2, 3, 4).sharedPrefix(List(1, 2, 4, 3)) === (List(1, 2), List(3, 4), List(4, 3))
  }

  test("fraction") {

    assert(nil[Int].fraction(_ ⇒ true).isNaN)
    assert(0.0 == List(1).fraction(_ < 1))


    assert(1.0 == List(0).fraction(_ < 1))
    assert(0.5 == List(0, 1).fraction(_ < 1))
  }

  test("batchBy()") {
    nil[Int].batchBy(_ ⇒ true) === Nil

         List(1 → 1, 1 → 2,       2 → 1,       1 → 3,       2 → 2, 2 → 3).batchBy(_._1) ===
    List(List(1 → 1, 1 → 2), List(2 → 1), List(1 → 3), List(2 → 2, 2 → 3))
  }

  test("batchWhile") {
    List("foo", "food", "bar", "bare", "barf")
      .batchWhile(_.mkString("").distinct.length <= 4) === List(List("foo", "food"), List("bar", "bare"), List("barf"))
  }

  test("prefixPadTo") {  List(1, 2, 3).prefixPadTo(6, 0) === List(0, 0, 0, 1, 2, 3) }

  test("tap") {
    strings().run(ss ⇒ nil[Int].tap(ss += "empty", _ ⇒ ss += "non-empty")) === List("empty")
    strings().run(ss ⇒  List(1).tap(ss += "empty", _ ⇒ ss += "non-empty")) === List("non-empty")
  }

  test("tapEmpty") {
    strings().run(ss ⇒ nil[Int].tapEmpty(ss += "empty")) === List("empty")
    strings().run(ss ⇒  List(1).tapEmpty(ss += "empty")) === Nil
  }

  test("tapNonEmpty") {
    strings().run(ss ⇒ nil[Int].tapNonEmpty(_ ⇒ ss += "non-empty")) === Nil
    strings().run(ss ⇒  List(1).tapNonEmpty(_ ⇒ ss += "non-empty")) === List("non-empty")
  }

  test("amass") { List(1, 2, 3, 4).amass { case i if i % 2 == 0 ⇒ List(i, -i) } === List(2, -2, 4, -4) }

  test("interleave") { List(1, 2, 3).interleave(List(10, 20)) === List(1, 10, 2, 20, 3) }

  test("interleaveWith") {
    List(1, 2, 3).interleaveWith(List("ten", "twenty"))(_.valueOr(_.toString)) === List("1", "ten", "2", "twenty", "3")
  }

  test("cartesianProduct") {
    List(List(1, 2), List(10, 20), List(100, 200)).cartesianProduct ===
      (for {a ← List(1, 2); b ← List(10, 20); c ← List(100, 200)} yield List(a, b, c))
  }

  test("onlyOrThrow")  {
    on(nil[Int], List(1, 2)).calling(_.onlyOrThrow(exception)).throws("List()", "List(1, 2)")
    List(1).onlyOrThrow(_ ⇒ new Exception()) === 1
  }

  test("onlyEither") {
    on(nil[Int], List(1, 2), List(1)).calling(_.onlyEither).produces(Left(Nil), Left(List(1, 2)), Right(1))
  }

  test("onlyOrEither") {
    on(nil[Int], List(1, 2), List(1)).calling(_.onlyOrEither(_.size)).produces(Left(0), Left(2), Right(1))
  }

  test("onlyOption") { on(nil[Int], List(1, 2), List(1)).calling(_.onlyOption).produces(None, None, Some(1)) }

  test("zipExact") {
    Nil.zipExact(Nil)                     === (Nil, None)
    List(1, 2, 3).zipExact(List(4, 5, 6)) === (List((1, 4), (2, 5), (3, 6)), None)
    List(1, 2, 3).zipExact(Nil)           === (Nil, Some(Left(List(1, 2, 3))))
    Nil.zipExact(List(4, 5, 6))           === (Nil, Some(Right(List(4, 5, 6))))
    List(1, 2, 3).zipExact(List(4))       === (List((1, 4)), Some(Left(List(2, 3))))
    List(1).zipExact(List(4, 5, 6))       === (List((1, 4)), Some(Right(List(5, 6))))
  }

  test("zipExactWith") {
    nil[Int].zipExactWith(nil[Int])(_ + _)           === (nil[Int], None)
    List(1, 2, 3).zipExactWith(List(4, 5, 6))(_ + _) === (List(5, 7, 9), None)
    List(1, 2, 3).zipExactWith(nil[Int])(_ + _)      === (nil[Int], Some(Left(List(1, 2, 3))))
    nil[Int].zipExactWith(List(4, 5, 6))(_ + _)      === (nil[Int], Some(Right(List(4, 5, 6))))
    List(1, 2, 3).zipExactWith(List(4))(_ + _)       === (List(5), Some(Left(List(2, 3))))
    List(1).zipExactWith(List(4, 5, 6))(_ + _)       === (List(5), Some(Right(List(5, 6))))

    List(1).zipExactWith(List(4, 5, 6))(_ + _, _ * -10, _ * 10)       === List(5, 50, 60)
    List(4, 5, 6).zipExactWith(List(1))(_ + _, _ * -10, _ * 10)       === List(5, -50, -60)
  }

  test("partitionEithers") {
    List(Left(1), Right("abc"), Right("def"), Left(2)).partitionEithers[List] === (List(1, 2), List("abc", "def"))
  }

  test("toMultiMap") {
    on(List((1, 10), (1, 11), (2, 20), (2, 21)))
      .calling(_.toMultiMap[List], _.toMultiMap[Set])
      .produces(Map(1 → List(10, 11), 2 → List(20, 21)), Map(1 → Set(10, 11), 2 → Set(20, 21)))
  }

  test("sortPromoting") {
    List("red", "green", "black", "purple").sortPromoting("purple", "green") === List("purple", "green", "black", "red")
  }

  test("sortDemoting") {
    List("black", "red", "purple", "green").sortDemoting("green", "black") === List("purple", "red", "green", "black")
  }

  test("shuffle") { List.range(1, 10).shuffle().sorted === List.range(1, 10) }

  test("updateIf") {
    val isEven = (x: Int) => x % 2 == 0
    val decrement = (x: Int) => x-1
    List.range(1, 6).updateIf(isEven, decrement) === List(1, 1, 3, 3, 5)
  }

  test("update") {
    List.range(1, 6).update { case x if x % 2 == 0 => x - 1 } === List(1, 1, 3, 3, 5)
  }
}
