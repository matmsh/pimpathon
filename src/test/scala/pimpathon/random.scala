package pimpathon

import scala.util.Random

import pimpathon.random._


class RandomTest extends PimpathonSuite {
  test("between") { assert(Random.between(20, 80) >= 20 && Random.between(20, 80) < 80) }
}