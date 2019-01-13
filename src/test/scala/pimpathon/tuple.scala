package pimpathon

import scala.language.implicitConversions

import pimpathon.builder._
import pimpathon.tuple._
import pimpathon.util._


class TupleTest extends PimpathonSuite {
  test("tap")   { strings().run(ss ⇒ (1, "foo").tap(i ⇒ s ⇒ ss += (s + i))) === List("foo1")}
  test("calc")  { ("123", "abc").calc(_ + _) === "123abc" }
  test("calcC") { ("123", "abc").calcC(a ⇒ b ⇒ a + b) === "123abc" }

  test("to") {
    implicit def intToString(i: Int): String = i.toString
    implicit def doubleToString(d: Double): String = d.toString

    (123, 456.0).to[String] === ("123", "456.0")
  }

  test("tmap") { (2, "abc").tmap(_ * 3, _.reverse) === (6, "cba") }

  test("map1") { (2, "abc").map1(_ * 3) === (6, "abc") }

  test("map2") { (2, "abc").map2(_.reverse) === (2, "cba")}

  test("addTo") {
    (ints(), strings()).tap(is ⇒ ss ⇒ (1, "foo").addTo(is, ss)).tmap(_.result(), _.result()) === (List(1), List("foo"))
  }

  test("removeFrom") {
    (ints(1), strings("foo")).tap(is ⇒ ss ⇒ (1, "foo").removeFrom(is, ss)).tmap(_.toList, _.toList) === (Nil, Nil)
  }
}