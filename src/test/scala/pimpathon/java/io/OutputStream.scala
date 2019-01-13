package pimpathon.java.io

import java.io._
import java.util.zip.GZIPInputStream


import scala.util.{Failure, Success}
import pimpathon.{PimpathonSuite, util}
import pimpathon.util._
import pimpathon.any._


class OutputStreamTest extends PimpathonSuite {

  test("attemptClose"){
    createOutputStream().attemptClose() === Success(())
    new ByteArrayOutputStream() { override def close() = goBoom }.attemptClose() === Failure(boom)
  }

  test("closeAfter") {
    val os = createOutputStream()

    os.closeAfter(_ ⇒ "result") === "result"
    os.assertClosed
  }

  test("closeI") {
    createOutputStream().closeIf(condition = false).assertOpen
    createOutputStream().closeIf(condition = true).assertClosed
  }

  test("closeUnless") {
    createOutputStream().closeUnless(condition = true).assertOpen
    createOutputStream().closeUnless(condition = false).assertClosed
  }

  test("drain")  {
    for { closeIn ← List(false, true); closeOut ← List(false, true); input ← List("Input", "Repeat" * 100) } {
      val (is, os) = (createInputStream(input), createOutputStream())

      os.drain(is, closeOut, closeIn)

      os.toString === input
      if (closeOut) os.assertClosed else os.assertOpen
      if (closeIn)  is.assertClosed else is.assertOpen
    }

    ignoreExceptions { // Merely verifying (via compilation) that named parameters works, bit redundant.
      val (is, os) = (createInputStream(), createOutputStream())

      os.drain(is, closeOut = false)
      os.drain(is, closeIn = false)
      os.drain(is, closeOut = false, closeIn = false)
      os.drain(is, closeIn = false, closeOut = false)
    }
  }

  test("<<") {
    val (is, os) = (createInputStream("content"), createOutputStream())

    os << is

    os.toString === "content"
    os.assertOpen
    is.assertOpen
  }

  test("buffered") {
    val (is, os) = (createInputStream("content"), createOutputStream())

    os.tap(o ⇒ (o.buffered: BufferedOutputStream).drain(is)).toString === "content"
  }

  test("gzip") {
    val os     = createOutputStream().tap(_.gzip.closeAfter(_.write("content".getBytes)))
    val result = createOutputStream().tap(rs ⇒ new GZIPInputStream(createInputStream(os.toByteArray)).drain(rs))

    result.toString === "content"
  }

  test("writeUpToN") {
    def write(text: String, n: Int): String = {
      val (is, os) = (createInputStream(text), createOutputStream())
      os.tap(_.writeUpToN(is, n), _.close()).toString
    }

    write("contents", 4) === "cont"
    write("contents", 8) === "contents"
    write("contents", 9) === "contents"
    write("contents", 0) === ""

    util.assertThrows[IllegalArgumentException]("requirement failed: You can't read a negative number of bytes!") {
      write("contents", -1)
    }
  }

  test("writeN") {
    def write(text: String, n: Int): String = {
      val (is, os) = (createInputStream(text), createOutputStream())
      os.tap(_.writeN(is, n), _.close()).toString
    }

    write("contents", 4) === "cont"
    write("contents", 8) === "contents"
    write("contents", 0) === ""

    util.assertThrows[IllegalArgumentException]("requirement failed: You can't read a negative number of bytes!") {
      write("contents", -1)
    }

    util.assertThrows[IOException]("Failed to write 9 only 8 were available")(write("contents", 9))
  }
}