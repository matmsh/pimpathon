package pimpathon

import pimpathon.util._


class ThreadLocalTest extends PimpathonSuite {
  test("create"){ threadLocal.create(1).get() === 1}
}