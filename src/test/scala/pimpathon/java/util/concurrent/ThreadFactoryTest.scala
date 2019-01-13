package pimpathon.java.util.concurrent

import java.util.concurrent.ThreadFactory

import pimpathon.PimpathonSuite
import pimpathon.any._
import pimpathon.java.util.concurrent.threadFactory._
import pimpathon.util._


class ThreadFactoryTest extends PimpathonSuite {
  test("naming") { basic.naming("name:" + _).calc(factory ⇒ {
    factory.newThread(runnable).getName === "name:0"
    factory.newThread(runnable).getName === "name:1"
  }) }

  private val basic    = new ThreadFactory { def newThread(r: Runnable): Thread = new Thread(r) }
  private val runnable = pimpathon.runnable.create(())
}
