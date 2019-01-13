package pimpathon

import scala.collection.{mutable ⇒ M}

import pimpathon.any._
import pimpathon.boolean._
import pimpathon.map._
import pimpathon.util._


class MapTest extends PimpathonSuite {
  test("containsAny"){
    assert(!empty.containsAny(None))
    assert(!empty.containsAny(Some(1)))
    assert(!nonEmpty.containsAny(None))
    assert(!nonEmpty.containsAny(Some(2)))
    assert(nonEmpty.containsAny(Some(1)))

    assert(!empty.containsAny(Nil))
    assert(!empty.containsAny(List(1)))
    assert(!nonEmpty.containsAny(Nil))
    assert(!nonEmpty.containsAny(List(2)))
    assert(nonEmpty.containsAny(List(1)))
    assert(nonEmpty.containsAny(List(1, 2)))
  }

  test("containsAll") {
    assert(empty.containsAll(None))
    assert(!empty.containsAll(Some(1)))
    assert(nonEmpty.containsAll(None))
    assert(!nonEmpty.containsAll(Some(2)))
    assert(nonEmpty.containsAll(Some(1)))

    assert(empty.containsAll(Nil))
    assert(!empty.containsAll(List(1)))
    assert(nonEmpty.containsAll(Nil))
    assert(!nonEmpty.containsAll(List(2)))
    assert(nonEmpty.containsAll(List(1)))
    assert(!nonEmpty.containsAll(List(1, 2)))
  }

  test("get") {
    Map.empty[Int, Int].get(Some(1)) === None
    Map.empty[Int, Int].get(None)    === None
    Map(1 → 2).get(None)             === None
    Map(1 → 2).get(Some(2))          === None
    Map(1 → 2).get(Some(1))          === Some(2)
  }

  test("getOrThrow") {
    Map(0 → "present").getOrThrow(0, "missing")                === "present"
    Map(0 → "present").getOrThrow(0, new Exception("missing")) === "present"
    Map(0 → "present").getOrThrow(0, util.goBoom: Exception)   === "present"

    util.assertThrows[IllegalArgumentException]("missing")(nonEmpty.getOrThrow(0, "missing"))
    util.assertThrows[RuntimeException]("missing")(nonEmpty.getOrThrow(0, new RuntimeException("missing")))
  }

  test("uncons") {
    on(empty, nonEmpty).calling(_.uncons("empty", _ ⇒ "nonEmpty")).produces("empty", "nonEmpty")
  }

  test("calcIfNonEmpty") {
    on (empty, nonEmpty).calling (_.calcIfNonEmpty (_ ⇒ "nonEmpty") ).produces (None, Some ("nonEmpty") )
  }

  test("emptyTo") { on(empty, Map(3 → 4)).calling(_.emptyTo(nonEmpty)).produces(nonEmpty, Map(3 → 4))}

  test("entryFor_maxKey") {
    on(Map.empty[Int, String], Map(1 → "min", 2 → "max")).calling(_.entryFor.maxKey).produces(None, Some(2 → "max"))
  }

  test("entryFor_minKey") {
    on(Map.empty[Int, String], Map(1 → "min", 2 → "max")).calling(_.entryFor.minKey).produces(None, Some(1 → "min"))
  }

  test("entryFor_maxValue") {
    on(Map.empty[Int, String], Map(1 → "abc", 2 → "def")).calling(_.entryFor.maxValue).produces(None, Some(2 → "def"))
  }

  test("entryFor_minValue") {
    on(Map.empty[Int, String], Map(1 → "abc", 2 → "def")).calling(_.entryFor.minValue).produces(None, Some(1 → "abc"))
  }

  test("valueFor_maxKey") {
    on(Map.empty[Int, String], Map(1 → "min", 2 → "max")).calling(_.valueFor.maxKey).produces(None, Some("max"))
  }

  test("valueFor_minKey") {
    on(Map.empty[Int, String], Map(1 → "min", 2 → "max")).calling(_.valueFor.minKey).produces(None, Some("min"))
  }

  test("keyFor_maxValue") {
    on(Map.empty[Int, String], Map(1 → "abc", 2 → "def")).calling(_.keyFor.maxValue).produces(None, Some(2))
  }

  test("keyFor_minValue") {
    on(Map.empty[Int, String], Map(1 → "abc", 2 → "def")).calling(_.keyFor.minValue).produces(None, Some(1))
  }

  test("mapKeysEagerly") {
    val originalKeysSeen = ints()
    def update(v: Int) = { originalKeysSeen += v; v * 10 }

    val result = Map(1 → 1, 2 → 2).mapKeysEagerly(update)
    assert( List(1, 2)== originalKeysSeen.toList, "Should have iterated over the original map already" )

    result === Map(10 → 1, 20 → 2)
    result === Map(10 → 1, 20 → 2)
    assert(List(1, 2) == originalKeysSeen.toList, "Shouldn't have iterated over the original map twice")
  }

  test("mapValuesEagerly") {
    val originalValuesSeen = ints()
    def update(v: Int) = { originalValuesSeen += v; v * 10 }

    val result = Map(1 → 1, 2 → 2).mapValuesEagerly(update)
    assert( List(1, 2) == originalValuesSeen.toList, "Should have iterated over the original map already" )

    result === Map(1 → 10, 2 → 20)
    result === Map(1 → 10, 2 → 20)
    assert(List(1, 2) == originalValuesSeen.toList, "Shouldn't have iterated over the original map twice")
  }

  test("mapValuesWithKey") {
    Map(1 → 2, 2 → 4).mapValuesWithKey(k ⇒ v ⇒ s"$k: $v") === Map(1 → "1: 2", 2 → "2: 4")
  }

  test("mapEntries") {
    Map(1 → 2, 2 → 4).mapEntries(k ⇒ v ⇒ (k.toString, v.toDouble)) === Map("1" → 2.0, "2" → 4.0)
  }

  test("seqMapKeys") {
    on(Map(2 → 4, 4 → 6), Map(1 → 3))
      .calling(_.seqMapKeys(k ⇒ (k % 2 == 0).option(k / 2))).produces(Some(Map(1 → 4, 2 → 6)), None)
  }

  test("seqMapValues") {
    on(Map(2 → 4, 4 → 6), Map(1 → 3))
      .calling(_.seqMapValues(v ⇒ (v % 2 == 0).option(v / 2))).produces(Some(Map(2 → 2, 4 → 3)), None)
  }

  test("seqMapEntries") {
    on(Map(2 → 4, 4 → 6), Map(1 → 3))
      .calling(_.seqMapEntries(k ⇒ v ⇒ (k % 2 == 0).option((k / 2) → (v / 2)))).produces(Some(Map(1 → 2, 2 → 3)), None)
  }

  test("findKey") {
    empty.findKey(_ ⇒ true)     === None
    nonEmpty.findKey(_ ⇒ false) === None
    nonEmpty.findKey(_ == 1)    === Some(1)
  }

  test("findValue") {
    empty.findValue(_ ⇒ true)      === None
    nonEmpty.findValue(_ ⇒ false)  === None
    nonEmpty.findValue(_ == 2)     === Some(2)
  }

  test("entryFor_matchingKey") {
    empty.entryFor.matchingKey(_ ⇒ true)     === None
    nonEmpty.entryFor.matchingKey(_ ⇒ false) === None
    nonEmpty.entryFor.matchingKey(_ == 1)    === Some(1 → 2)
  }

  test("entryFor_matchingValue") {
    empty.entryFor.matchingValue(_ ⇒ true)     === None
    nonEmpty.entryFor.matchingValue(_ ⇒ false) === None
    nonEmpty.entryFor.matchingValue(_ == 2)    === Some(1 → 2)
  }

  test("filterKeysNot") {
    empty.filterKeysNot(_ ⇒ true)           === empty
    nonEmpty.filterKeysNot(_ ⇒ true)        === empty
    nonEmpty.filterKeysNot(_ ⇒ false)       === nonEmpty
    Map(1 → 2, 2 → 3).filterKeysNot(_ == 2) === nonEmpty
  }

  test("filterValuesNot") {
    empty.filterValuesNot(_ ⇒ true)           === empty
    nonEmpty.filterValuesNot(_ ⇒ true)        === empty
    nonEmpty.filterValuesNot(_ ⇒ false)       === nonEmpty
    Map(1 → 2, 2 → 3).filterValuesNot(_ == 3) === nonEmpty
  }

  test("filterValues") {
    empty.filterValues(_ ⇒ true)           === empty
    nonEmpty.filterValues(_ ⇒ false)       === empty
    nonEmpty.filterValues(_ ⇒ true)        === nonEmpty
    nonEmpty.filterValues(_ ⇒ true)        === nonEmpty
    Map(1 → 2, 2 → 3).filterValues(_ == 2) === nonEmpty
  }

  test("keyExists") {
    assert(!empty.keyExists(_ ⇒ true))
    assert(!nonEmpty.keyExists(_ ⇒ false))
    assert(!nonEmpty.keyExists(_ == 2))
    assert(nonEmpty.keyExists(_ == 1))
  }

  test("valueExists") {
    assert(!empty.valueExists(_ ⇒ true))
    assert(!nonEmpty.valueExists(_ ⇒ false))
    assert(!nonEmpty.valueExists(_ == 1))
    assert(nonEmpty.valueExists(_ == 2))
  }

  test("containsEntry") {
    assert(!empty.containsEntry(1, 2))
    assert(!empty.containsEntry((1, 2)))
    assert(nonEmpty.containsEntry(1, 2))
    assert(nonEmpty.containsEntry((1, 2)))
    assert(!nonEmpty.containsEntry(1, 1))
    assert(!nonEmpty.containsEntry((1, 1)))
    assert(!nonEmpty.containsEntry(2, 2))
    assert(!nonEmpty.containsEntry((2, 2)))
  }

  test("mutable") { on(Map(1 → 2)).calling(_.mutable, _.toMutable).produces(M.Map(1 → 2), M.Map(1 → 2))}

  test("reverseToMultiMap") { Map(1 → 2, 2 → 2).reverseToMultiMap === Map(2 → Set(1, 2)) }

  test("reverse") { Map(1 → 2, 2 → 2).reverse(_.min) === Map(2 → 1)}

  test("sorted") { Map(1 → 2, 3 → 4).sorted(Ordering.Int.reverse).toList === List(3 → 4, 1 → 2)}

  test("sortBy") { Map(1 → 2, 3 → 4).sortBy(k ⇒ -k).toList === List(3 → 4, 1 → 2) }

  test("andThenM") {
    Map(1 → 10, 2 → 20, 3 → 30).andThenM(Map(10 → 100, 20 → 200, 40 → 400)) === Map(1 → 100, 2 → 200)
  }

  test("composeM") {
    Map(10 → 100, 20 → 200, 40 → 400).composeM(Map(1 → 10, 2 → 20, 3 → 30)) === Map(1 → 100, 2 → 200)
  }

  test("partitionKeys") {
    Map(1 → 2, 2 → 3).partitionKeys(_ == 1) === (Map(1 → 2), Map(2 → 3))
  }

  test("partitionValues") {
    Map(1 → 2, 2 → 3).partitionValues(_ == 2) === (Map(1 → 2), Map(2 → 3))
  }

  test("partitionKeysBy") {
    Map(1 → 2, 2 → 3).partitionKeysBy { case 1 ⇒ "foo" } === (Map(2 → 3), Map("foo" → 2))
  }

  test("partitionValuesBy") {
    Map(1 → 2, 2 → 3).partitionValuesBy { case 2 ⇒ "foo" } === (Map(2 → 3), Map(1 → "foo"))
  }

  test("partitionEntriesBy") {
    Map(1 → 2, 2 → 3).partitionEntriesBy { case (2, 3) ⇒ "foo" → "oof" } === (Map(1 → 2), Map("foo" → "oof"))
  }

  test("collectKeys") {  Map(1 → 2, 2 → 3).collectKeys { case 2 ⇒ "foo" } === Map("foo" → 3) }

  test("collectValues") { Map(1 → 2, 2 → 3).collectValues { case 2 ⇒ "foo" } === Map(1 → "foo") }

  test("updateValue") {
    on(nonEmpty).calling(
      _.updateValue(1, _ ⇒ None), _.updateValue(1, _ ⇒ Some(1)), _.updateValue(2, _ ⇒ None), _.updateValue(2, _ ⇒ Some(3))
    ).produces(empty, Map(1 → 1), nonEmpty, nonEmpty)
  }

  test("updateKeys") {
    nonEmpty.updateKeys[Int]((i: Int) ⇒ None)              === empty
    nonEmpty.updateKeys(k ⇒ Some(k * 2))                   === Map(2 → 2)
    Map(1 → 2, 2 → 3).updateKeys(k ⇒ k.filterSelf(_ == 1)) === nonEmpty
  }

  test("updateKeysPF") {
    nonEmpty.updateKeys(util.partial[Int, Int]())              === empty
    nonEmpty.updateKeys(util.partial(1 → 2))          === Map(2 → 2)
    Map(1 → 2, 2 → 3).updateKeys(util.partial(1 → 1)) === nonEmpty
  }

  test("updateValues") {
    nonEmpty.updateValues[Int]((i: Int) ⇒ None)              === empty
    nonEmpty.updateValues(v ⇒ Some(v * 2))                   === Map(1 → 4)
    Map(1 → 2, 2 → 3).updateValues(v ⇒ v.filterSelf(_ == 2)) === nonEmpty
  }

  test("updateValuesPF")  {
    nonEmpty.updateValues(util.partial[Int, Int]())     === empty
    nonEmpty.updateValues(util.partial(2 → 4))          === Map(1 → 4)
    Map(1 → 2, 2 → 3).updateValues(util.partial(2 → 2)) === nonEmpty
  }

  test("zipWith") {
    Map(1 → 2, 2 → 3).zipWith(Map(1 → 20, 3 → 30)) {
      case (Some(lhs), Some(rhs)) ⇒ lhs + rhs
      case (None, Some(rhs)) ⇒ rhs
    } === Map(1 → 22, 3 → 30)

  }

  private val (empty, nonEmpty) = (Map.empty[Int, Int], Map(1 → 2))
}
