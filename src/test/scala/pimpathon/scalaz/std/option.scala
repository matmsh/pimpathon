package pimpathon.scalaz.std


import pimpathon.PimpathonSuite
import scalaz.{Failure, Success, NonEmptyList => NEL}
import pimpathon.scalaz.std.option._

trait std
class OptionTest extends PimpathonSuite {
  
  test("toSuccessNel") { calling(_.toSuccessNel("fail")).produces(Success(1), Failure(NEL("fail")))  }
  test("toFailureNel") { calling(_.toFailureNel("succeed")).produces(Failure(NEL(1)), Success("succeed")) }

  private def calling[A](f: Option[Int] ⇒ A) = pimpathon.util.on(Some(1), None).calling(f)
}