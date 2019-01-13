package pimpathon.argonautTests

import _root_.argonaut._
import pimpathon.PimpathonSuite
import pimpathon.argonaut._
import pimpathon.util._


class DecodeJsonTest extends PimpathonSuite with JsonUtil {
  test("beforeDecode") {
    decoder.beforeDecode(reverse).decodeJson(json) === decoder.decodeJson(reverse(json))
  }

  test("compose") {
    decoder.compose(reverse).decodeJson(json) === decoder.decodeJson(reverse(json))
  }

  test("upcast") {
    Derived.codec.upcast[Base].decodeJson(derivedEncoded) === DecodeResult.ok(derived)
  }

  test("mapEntries") {
    mapDecoder.mapEntries(reverseEntry).decodeJson(jsonMap("foo" → "bar")) ===
      mapDecoder.decodeJson(jsonMap("oof" → "rab"))
  }


  test("mapKeys") {
    mapDecoder.mapKeys(_.reverse).decodeJson(jsonMap("foo" → "bar")) ===
      mapDecoder.decodeJson(jsonMap("oof" → "bar"))
  }

  test("mapValues") {
    mapDecoder.mapValues(_.reverse).decodeJson(jsonMap("foo" → "bar")) ===
      mapDecoder.decodeJson(jsonMap("foo" → "rab"))
  }

}
