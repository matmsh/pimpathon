package pimpathon.frills

import pimpathon.PimpathonSuite
import pimpathon.frills.pimpTry._
import pimpathon.util._

import scala.util.{Failure, Success}
import scalaz.{-\/, \/-}


class TryTest extends PimpathonSuite  {
  test("toDisjunction") {
    on(Success("foo"), Failure(boom)).calling(_.toDisjunction).produces(\/-("foo"), -\/(boom))
  }
}