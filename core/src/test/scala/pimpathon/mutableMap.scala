package pimpathon

import org.junit.Assert._
import org.junit.Test
import pimpathon.mutableMap._

import scala.collection.{mutable => M}


class MutableMapTest {
  @Test def retainKeys(): Unit = {
    assertEquals(empty,    empty.retainKeys(_ => false))
    assertEquals(empty,    nonEmpty.retainKeys(_ => false))
    assertEquals(nonEmpty, nonEmpty.retainKeys(_ => true))
    assertEquals(nonEmpty, M.Map(1 -> 2, 2 -> 3).retainKeys(_ == 1))
  }

  @Test def retainValues(): Unit = {
    assertEquals(empty,    empty.retainValues(_ => false))
    assertEquals(empty,    nonEmpty.retainValues(_ => false))
    assertEquals(nonEmpty, nonEmpty.retainValues(_ => true))
    assertEquals(nonEmpty, M.Map(1 -> 2, 2 -> 3).retainValues(_ == 2))
  }

  private def empty    = M.Map[Int, Int]()
  private def nonEmpty = M.Map[Int, Int](1 → 2)
}