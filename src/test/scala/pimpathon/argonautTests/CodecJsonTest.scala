package pimpathon.argonautTests

import _root_.argonaut._
import org.scalatest.FunSuite
import pimpathon.PimpathonSuite
import pimpathon.any._
import pimpathon.argonaut._
import pimpathon.util._
import sjc.delta.argonaut.json.actualExpected.flat._
import sjc.delta.argonaut.matchers._
import sjc.delta.matchers.syntax.anyDeltaMatcherOps
import scalaz.{-\/, \/, \/-}




class CodecJsonTest extends PimpathonSuite with JsonUtil {
  test("beforeDecode") {codec.beforeDecode(reverse).decodeJson(json) === codec.decodeJson(reverse(json))}

  test("afterDecode") {codec.afterDecode(_.reverse).decodeJson(json) === reverse(codec.decodeJson(json))}

  test("beforeEncode") {codec.beforeEncode(_.reverse).encode(list) <=> codec.encode(list.reverse)}

  test("afterEncode") {codec.afterEncode(reverse).encode(list) <=> reverse(codec.encode(list))}

  test("andThen") {codec.andThen(reverse).encode(list) <=> reverse(codec.encode(list))}

  test("compose") {codec.compose(reverse).decodeJson(json) === codec.decodeJson(reverse(json))}

  test("xmapEntries") {
    mapCodec.xmapEntries[String, String](reverseEntry)(reverseEntry).calc(reversed ⇒ {
      reversed.encode(Map("foo" → "bar")) <=> mapCodec.encode(Map("oof" → "rab"))
      reversed.decodeJson(jsonMap("foo" → "bar")) === mapCodec.decodeJson(jsonMap("oof" → "rab"))
    })
  }

  test("xmapKeys") {
    mapCodec.xmapKeys[String](_.reverse)(_.reverse).calc(reversed ⇒ {
      reversed.encode(Map("foo" → "bar")) <=> mapCodec.encode(Map("oof" → "bar"))
      reversed.decodeJson(jsonMap("foo" → "bar")) === mapCodec.decodeJson(jsonMap("oof" → "bar"))
    })
  }

  test("xmapValues") {
    mapCodec.xmapValues[String](_.reverse)(_.reverse).calc(reversed ⇒ {
      reversed.encode(Map("foo" → "bar")) <=> mapCodec.encode(Map("foo" → "rab"))
      reversed.decodeJson(jsonMap("foo" → "bar")) === mapCodec.decodeJson(jsonMap("foo" → "rab"))
    })
  }

  test("xmapDisjunction") {
    stringCodec.xmapDisjunction[Int](attempt(_.toInt))(_.toString).calc(intCodec ⇒ {
      intCodec.encode(3) <=> Json.jString("3")
      intCodec.decodeJson(Json.jString("3")) === DecodeResult.ok(3)
      intCodec.decodeJson(Json.jString("a")) === DecodeResult.fail("a", CursorHistory(Nil))
    })
  }

  // Searching for a better name before making this a pimp (and one producing Either[A, B])
  private def attempt[A, B](f: A => B)(a: A): A \/ B = a.attempt(f).fold(_ => -\/(a), \/-(_))

}
