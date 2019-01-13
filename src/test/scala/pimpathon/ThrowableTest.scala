package pimpathon

import pimpathon.throwable._
import pimpathon.util._


class ThrowableTest extends PimpathonSuite {
  test("stackTraceAsString") {
    boom.stackTraceAsString().lines.toList.take(4) ===
      boom.toString :: boom.getStackTrace.toList.take(3).map("\tat " + _)
  }
}