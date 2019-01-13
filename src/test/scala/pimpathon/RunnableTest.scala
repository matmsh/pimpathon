package pimpathon

import pimpathon.builder._
import pimpathon.runnable._
import pimpathon.util._


class RunnableTest extends PimpathonSuite {
  test("create"){ ints().run(is ⇒ run(runnable.create(is += 1))) === List(1) }
  test("fromThunk"){ ints().run(is ⇒ run(() ⇒ is += 3)) === List(3) }

  private def run(runnable: Runnable): Unit = runnable.run()
}