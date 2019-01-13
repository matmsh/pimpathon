package pimpathon.argonautTests

import argonaut.StringWrap.StringToStringWrap
import pimpathon.PimpathonSuite
import pimpathon.argonaut._
import sjc.delta.argonaut.json.actualExpected.flat._
import sjc.delta.argonaut.matchers._
import sjc.delta.matchers.syntax.anyDeltaMatcherOps


class EncodeJsonTest extends PimpathonSuite with JsonUtil {
  test("afterEncode") {
    encoder.afterEncode(reverse).encode(list) <=> reverse(encoder.encode(list))
  }

  test("andThen") {
    encoder.andThen(reverse).encode(list) <=> reverse(encoder.encode(list))
  }

  test("downcast") {
    Base.encoder.downcast[Derived].encode(derived) <=> derivedEncoded
  }

  test("add") {
    encoder.add("length" := _.length).encode(list) <=> encoder.encode(list).addIfMissing("length" := list.length)
  }

  test("contramapEntries") {
    mapEncoder.contramapEntries[String, String](reverseEntry).encode(Map("foo" → "bar")) <=> mapEncoder.encode(Map("oof" → "rab"))
  }

  test("contramapKeys") {
    mapEncoder.contramapKeys[String](_.reverse).encode(Map("foo" → "bar")) <=> mapEncoder.encode(Map("oof" → "bar"))
  }

  test("contramapValues") {
    mapEncoder.contramapValues[String](_.reverse).encode(Map("foo" → "bar")) <=> mapEncoder.encode(Map("foo" → "rab"))
  }

}