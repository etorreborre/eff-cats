package org.atnos.eff.addon.monix

import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2._
import org.specs2.concurrent.ExecutionEnv

import scala.collection.mutable.ListBuffer
import monix.execution.Scheduler.Implicits.global
import monix.eval.Task
import org.atnos.eff._
import org.atnos.eff.addon.monix.task._
import org.atnos.eff.syntax.addon.monix.task._

import scala.concurrent.Await
import scala.concurrent.duration._

class TaskEffectSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck { def is = "monix task".title ^ sequential ^ s2"""

 Tasks can work as normal values                           $e1
 Task effects can be attempted                             $e2
 Task effects can be executed concurrently                 $e3 ${tag("travis")}
 Task effects are stacksafe with recursion                 $e5
 A forked task computation can be timed out                $e6

 Simple Task calls can be memoized                 $e7
 Attempted Task calls can be memoized              $e8
 Simple Task calls with timeout can be memoized    $e9
 Attempted Task calls with timeout can be memoized $e10
 Failed tasks must not be memoized                 $e11

 Async boundaries can be introduced between computations $e12
 Task effect is stacksafe with traverseA                 $e13

## RETRIES

 An effect can be retried until a condition becomes true          $retry1
 It will return the latest value if the condition is still false
 after all the durations have expired                             $retry2

"""

  type S = Fx.fx2[Task, Option]

  def e1 = {
    def action[R :_task :_option]: Eff[R, Int] = for {
      a <- taskDelay(10)
      b <- taskDelay(20)
    } yield a + b

    action[S]().runOption.runSequential.runToFuture must beSome(30).await(retries = 5, timeout = 5.seconds)
  }

  def e2 = {
    def action[R :_task :_option]: Eff[R, Int] = for {
      a <- taskDelay(10)
      b <- taskDelay { boom; 20 }
    } yield a + b

    action[S]().taskAttempt.runOption.runSequential.runToFuture must beSome(beLeft(boomException)).await(retries = 5, timeout = 5.seconds)
  }

  def e3 = {
    val messages: ListBuffer[Int] = new ListBuffer[Int]
    val delays = List(600, 200, 400, 800)

    def action(i: Int): Eff[S, Unit] =
      taskFork(Task.delay {
        Thread.sleep(i.toLong)
        messages.append(i)
      })

    val run = taskDelay[S, Unit](Thread.sleep(1000)) >> Eff.traverseA(delays)(action)

    eventually(retries = 5, sleep = 0.seconds) {
      messages.clear()
      Await.result(run.runOption.runAsync.runToFuture, 3.seconds)

      "the messages are ordered" ==> {
        messages.toList ==== delays.sorted
      }
    }

  }

  def e5 = {
    type R = Fx.fx1[Task]

    def loop(i: Int): Task[Eff[R, Int]] =
      if (i == 0) Task.now(Eff.pure(1))
      else Task.now(taskSuspend(loop(i - 1)).map(_ + 1))

    Await.result(taskSuspend(loop(100000)).runSequential.runToFuture, Duration.Inf) must not(throwAn[Exception])
  }

  def e6 = {
    taskDelay({ sleepFor(10000.millis); 1 }, timeout = Some(50.millis)).taskAttempt.runSequential.runToFuture must beLeft[Throwable].awaitFor(20.seconds)
  }

  def e7 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = taskMemo("only once", cache, taskDelay({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).runSequential.runToFuture must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e8 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = taskMemo("only once", cache, taskDelay({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).taskAttempt.runSequential.runToFuture must beRight(1).await
    invocationsNumber must be_==(1)
  }

  def e9 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = taskMemo("only once", cache, taskDelay({ invocationsNumber += 1; 1 }, timeout = Option(10000.millis)))

    (makeRequest >> makeRequest).runSequential.runToFuture must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e10 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    def makeRequest = taskDelay({ invocationsNumber += 1; 1 }, timeout = Option(10000.millis)).taskMemo("only once", cache)
      (makeRequest >> makeRequest).taskAttempt.runSequential.runToFuture must beRight(1).await

    invocationsNumber must be_==(1)
  }

  def e11 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    var firstTime = true

    def makeRequest =
      if (firstTime)
        taskMemo("only once", cache, taskDelay { firstTime = false; throw new Exception("") } >> taskDelay({ invocationsNumber += 1; 1 }))
      else
        taskMemo("only once", cache, taskDelay { invocationsNumber += 1; 1 })

    makeRequest.taskAttempt.runSequential.runToFuture must beLeft[Throwable].await
    makeRequest.taskAttempt.runSequential.runToFuture must beRight(1).await

    invocationsNumber must be_==(1)
  }

  def e12 = {
    def action[R :_task :_option]: Eff[R, Int] = for {
      a <- taskDelay(10).asyncBoundary
      b <- taskDelay(20)
    } yield a + b

    action[S]().runOption.runAsync.runToFuture must beSome(30).await(retries = 5, timeout = 5.seconds)
  }

  def e13 = {
    val action = (1 to 10000).toList.traverseA { i =>
      taskDelay(i)
    }

    action.runAsync must not(throwA[Throwable])
  }

  def retry1 = {
    type S = Fx2[Task, Option]
    var i = 0

    val action: Eff[S, Int] =
      taskDelay[S, Int] { sleepFor(10.millis); i += 1; i }

    val execute: Eff[S, Int] =
      action.retryUntil(i => i == 3, List(10.millis, 20.millis))

    execute.runOption.runAsync.runToFuture must beSome(3).await
  }

  def retry2 = {
    type S = Fx2[Task, Option]
    var i = 0

    val action: Eff[S, Int] =
      taskDelay[S, Int] { sleepFor(10.millis); i += 1; i }

    val execute: Eff[S, Int] =
      action.retryUntil(i => i == 5, List(10.millis, 20.millis))

    execute.runOption.runAsync.runToFuture must beSome(3).await
  }

  /**
   * HELPERS
   */
  def boom: Unit = throw boomException
  val boomException: Throwable = new Exception("boom")

  def sleepFor(duration: FiniteDuration) =
    try Thread.sleep(duration.toMillis) catch { case t: Throwable => () }

}

