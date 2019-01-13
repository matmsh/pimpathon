package pimpathon

import _root_.java.nio.charset.Charset

import pimpathon.string._
import pimpathon.util._


class StringTest extends PimpathonSuite {
  test("stripAffixes") {
    on("(foo)", "(foo", "oof)", "ooo").calling(_.stripAffixes("(", ")")).produces("foo", "foo", "oof", "ooo")
  }

  test("affixWith") {
    on("(foo)", "(foo", "oof)", "ooo").calling(_.affixWith("(", ")")).produces("(foo)", "(foo)", "(oof)", "(ooo)")
  }

  test("quote") {
    on("foo", "\"foo", "foo\"", "\"foo\"").calling(_.quote).produces("\"foo\"", "\"\"foo\"", "\"foo\"\"", "\"\"foo\"\"")
  }

  test("quoteWith") {
    on("foo", "'foo", "foo'", "'foo'").calling(_.quoteWith('\'')).produces("'foo'", "''foo'", "'foo''", "''foo''")
  }

  test("unquote") {
    on("foo", "\"foo", "foo\"", "\"foo\"").calling(_.unquote).produces("foo", "foo", "foo", "foo")
  }

  test("hyphenate") {
    on("foo", "fooFood").calling(_.hyphenate).produces("foo", "foo-food")
  }

  test("prefixWith"){
    "".prefixWith("") === ""
    on("", "-suffix", "prefix").calling(_.prefixWith("prefix")).produces("prefix", "prefix-suffix", "prefix")
  }

  test("sharedPrefix") {
    "".sharedPrefix("")         === ("", "", "")
    "1".sharedPrefix("1")       === ("1", "", "")
    "1234".sharedPrefix("1243") === ("12", "34", "43")
  }

  test("suffixWith") {
    "".suffixWith("") === ""
    on("", "prefix-", "suffix").calling(_.suffixWith("suffix")).produces("suffix", "prefix-suffix", "suffix")
  }

  test("prefixPadTo") {  "-suffix".prefixPadTo(10, 'p') === "ppp-suffix" }

  test("md5") { "blah".md5 === "6f1ed002ab5595859014ebf0951522d9" }

  test("emptyTo") { on("", "def").calling(_.emptyTo("abc")).produces("abc", "def")}

  test("toByteArray") {
    Array('a'.toByte, 'b'.toByte, 'c'.toByte) === "abc".toByteArray(Charset.forName("UTF-8"))
    Array('d'.toByte, 'e'.toByte, 'f'.toByte) === "def".toByteArray
  }

  test("wrap") {
    val wrapped =
      """|Pimpathon contains pimps
         |for classes in core
         |scala & java libraries
         |and pimps for external
         |libraries""".stripMargin

    wrapped.replaceAllLiterally("\n", " ").wrap(24) === wrapped
  }

  test("indent") {
    """|Pimpathon contains pimps
       |  for classes in core
       |    scala & java libraries
       |  and pimps for external
       |libraries""".stripMargin.indent ===
    """|  Pimpathon contains pimps
       |    for classes in core
       |      scala & java libraries
       |    and pimps for external
       |  libraries""".stripMargin
  }
}