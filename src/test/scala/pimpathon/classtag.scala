package pimpathon

import pimpathon.util._


class ClassTagTest extends  PimpathonSuite {
  test("className")       { classTag.className[ClassTagTest]       === "pimpathon.ClassTagTest" }
  test("simplecCassName") { classTag.simpleClassName[ClassTagTest] === "ClassTagTest" }
  test("klassOf")         { classTag.klassOf[ClassTagTest]         === classOf[ClassTagTest] }
}