package pimpathon.java.util

import java.util.concurrent.Callable

import pimpathon.PimpathonSuite
import pimpathon.java.util.callable._
import pimpathon.util
import pimpathon.util._

import scala.util.{Failure, Success}


class CallableTest extends PimpathonSuite {
  test("create") { call(callable.create(1)) === 1 }

  test("fromThunk") { call(() ⇒ 1) === 1 }

  test("attempt") {
    on(callable.create(3), callable.create(goBoom)).calling(_.attempt.call).produces(Success(3), Failure(boom))

    util.assertThrows[InterruptedException]("fatal") {
      callable.create[Int](throw new InterruptedException("fatal")).attempt.call()
    }
  }

  private def call[A](callable: Callable[A]): A = callable.call()
}