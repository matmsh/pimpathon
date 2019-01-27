package pimpathon.argonautTests

import argonaut._
import argonaut.Json._
import monocle.Traversal
import pimpathon.PimpathonSuite
import pimpathon.map._
import pimpathon.util._
import pimpathon.argonaut._
import sjc.delta.matchers.syntax.anyDeltaMatcherOps
import sjc.delta.argonaut.matchers._
import sjc.delta.argonaut.json.actualExpected.flat._


class TraversalFrills extends PimpathonSuite with JsonUtil {
  test("bool") {
    calling(id.bool.getAll)           .partitions(fields).into(lying → List(true), others → nil)
    calling(id.bool.modify(_ ⇒ false)).partitions(fields).into(lying → jBool(false), others → unchanged)
  }

  test("string"){
    calling(id.string.getAll)         .partitions(fields).into(name → List("Eric"),     others → nil)
    calling(id.string.modify(_ + "!")).partitions(fields).into(name → jString("Eric!"), others → unchanged)
  }

  test("array"){
    calling(id.array.getAll)           .partitions(fields)
      .into(address → List(acaciaRoad), potatoes → List(Nil), others → nil)

    calling(id.array.modify(_.reverse)).partitions(fields).into(address → jArray(acaciaRoad.reverse), others → unchanged)

    id.array.string.getAll(address)              === List(List("29 Acacia Road", "Nuttytown"))
    id.array.string.modify("Eric" :: _)(address) <=> jArray(jString("Eric") :: acaciaRoad)
    id.array.int.getAll(address)                 === List(List())
    id.array.int.modify(1 :: _)(address)         <=> jArrayElements(jNumber(1))
  }

  test("obj"){
    calling(id.obj.getAll).partitions(fields)
      .into(preferences → List(bananas), knownUnknowns → List(JsonObject.empty), awkward → List(intObj), others → nil)

    calling(id.obj.modify(_ - "bananas")).partitions(fields).into(preferences → jEmptyObject, others → unchanged)

    id.obj.bool.getAll(preferences) === List(Map("bananas" → true))
    id.obj.bool.modify(_ + ("Selina" → true))(preferences) === ("Selina" → jBool(true)) ->: preferences

    id.obj.obj.bool.getAll(jObjectFields("prefs" → preferences)) === List(Map("prefs" → Map("bananas" -> true)))

    id.obj.obj.bool.modify(_.mapKeysEagerly(_.capitalize))(jObjectFields("prefs" → preferences)) === jObjectFields(
      "Prefs" → preferences
    )
  }

//  test( double(): Unit = {
//    calling(id.double.getAll).partitions(fields).into(age → List(3.0), width → List(33.5), others → nil)
//
//    calling(id.double.modify(_ * 2.0)).partitions(fields)
//      .into(age → jNumberOrNull(6.0), width → jNumberOrNull(67.0), others → unchanged)
//  }

  test("int"){
    calling(id.int.getAll)       .partitions(fields).into(age → List(3),    others → nil)
    calling(id.int.modify(_ * 2)).partitions(fields).into(age → jNumber(6), others → unchanged)
  }

  test("descendant_values"){
    id.descendant("age").getAll(jobj) === List(age)
    id.descendant("age").modify(_ ⇒ redacted)(jobj) <=> ("age" → redacted) ->: jobj

    id.descendant("{name, age}").getAll(jobj) === List(name, age)
    id.descendant("{name, age}").modify(_ ⇒ redacted)(jobj) <=> ("name" → redacted) ->: ("age" → redacted) ->: jobj
  }

  test("descendant_elements"){
    id.descendant("[0, 2]").getAll(jArray(fields)) === List(lying, address)

    id.descendant("[0, 2]").modify(_ ⇒ redacted)(jArray(fields)) <=> jArrayElements(
      redacted, name, redacted, age, width, preferences, potatoes, knownUnknowns, awkward
    )
  }

  test("descendant_all"){
    id.descendant("*").getAll(jobj) === List(
      name, age, lying, address, preferences, width, potatoes, knownUnknowns, awkward
    )

    id.descendant("*").modify(_ ⇒ jString("redacted"))(jobj) <=> jObjectFields(
      "name" → redacted, "age" → redacted, "lying" → redacted, "address" → redacted, "preferences" → redacted,
      "width" → redacted, "potatoes" → redacted, "knownUnknowns" → redacted, "awkward" → redacted
    )
  }

  test("descendant_filter"){
    val repeatingThings = parse("""
    |{
    |  "repeatingThings" : [
    |    { "country" : "United Kingdom", "summary" : "Summarise this" },
    |    { "country" : "France",         "summary" : "Summarise that" }
    |  ]
    |}""".stripMargin)

    id.descendant("repeatingThings/*[country='United Kingdom']/summary").string.getAll(repeatingThings) === List(
      "Summarise this"
    )

    id.descendant("repeatingThings/*[country='United Kingdom']/summary").string.set("UK")(repeatingThings) === parse("""
    |{
    |  "repeatingThings" : [
    |    { "country" : "United Kingdom", "summary" : "UK" },
    |    { "country" : "France",         "summary" : "Summarise that" }
    |  ]
    |}""".stripMargin)
  }

  private val id: Traversal[Json, Json] = Traversal.id[Json]
}
