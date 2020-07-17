package org.atnos.eff

import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.future._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.future._
import org.specs2._
import org.specs2.concurrent.ExecutionEnv

import scala.collection.mutable.ListBuffer
import scala.concurrent._
import duration._
import org.specs2.matcher.ThrownExpectations

import scala.util.control._

class FutureEffectSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck with ThrownExpectations { def is = sequential ^ s2"""

 Future effects can work as normal values                      $e1
 Future effects can be attempted                               $e2
 Future effects can be executed concurrently                   $e3 ${tag("travis")}
 Future effects are stacksafe with recursion                   $e6
 An Future effect can be created from Either                   $e7
 An Future forked computation can be timed out                 $e8

 Simple Future calls can be memoized                 $e9
 Attempted Future calls can be memoized              $e10
 Simple Future calls with timeout can be memoized    $e11
 Attempted Future calls with timeout can be memoized $e12

 TimedFuture calls can be memoized with a memo effect $e13
 addLast can be used to make sure an action executes when a future fails $e14

 Future effects can be detached safely with detachA $e15

## RETRIES

 An effect can be retried until a condition becomes true          $retry1
 It will return the latest value if the condition is still false
 after all the durations have expired                             $retry2

"""

  type S = Fx.fx2[TimedFuture, Option]

  implicit val scheduler = ExecutorServices.schedulerFromScheduledExecutorService(ee.ses)
  implicit val ec = ee.ec

  def e1 = {
    def action[R :_future :_option]: Eff[R, Int] = for {
      a <- futureDelay(10)
      b <- futureDelay(20)
    } yield a + b

    action[S]().runOption.runSequential must beSome(30).await(retries = 5, timeout = 5.seconds)
  }

  def e2 = {
    def action[R :_future :_option]: Eff[R, Int] = for {
      a <- futureDelay(10)
      b <- futureDelay { boom; 20 }
    } yield a + b

    action[S]().futureAttempt.runOption.runSequential must beSome(beLeft(boomException)).await(retries = 5, timeout = 5.seconds)
  }

  def e3 = {
    val messages: ListBuffer[Int] = new ListBuffer[Int]
    val delays = List(600, 200, 400, 800)

    def action(i: Int): Eff[S, Unit] =
      futureDelay {
        Thread.sleep(i.toLong)
        messages.append(i)
      }

    val run = futureDelay[S, Unit](Thread.sleep(1000)) >> Eff.traverseA(delays)(action)
    eventually(retries = 5, sleep = 0.seconds) {
      messages.clear()
      Await.result(run.runOption.runAsync, 4 seconds)

      "the messages are ordered" ==> {
        messages.toList ==== delays.sorted
      }
    }
  }

  def e5 = {
    val list = (1 to 5000).toList
    type U = Fx.prepend[Choose, S]
    val action = list.traverseA(i => chooseFrom[U, Int](List(1)) >> futureDelay[U, String](i.toString))

    action.runChoose[List].runOption.map(_.map(_.flatten)).runSequential must beSome(list.map(_.toString)).await(retries = 5, timeout = 5.seconds)
  }

  def e6 = {
    type R = Fx.fx1[TimedFuture]

    def loop(i: Int): Future[Eff[R, Int]] =
      if (i == 0) Future.successful(Eff.pure(1))
      else        Future.successful(futureDefer(loop(i - 1)).flatten[Int].map(_ + 1))

    eventually(retries = 5, sleep = 0.seconds) {
      Await.result(futureDelay(loop(100000)).runSequential, Duration.Inf) must not(throwAn[Exception])
    }
  }

  def e7 = {
    futureFromEither(Left[Throwable, Int](boomException)).futureAttempt.runSequential must beLeft(boomException).awaitFor(1.second)
  }

  def e8 = {
    futureDelay({ sleepFor(10000.millis); 1 }, timeout = Some(50.millis)).futureAttempt.runSequential must beLeft.awaitFor(20.seconds)
  }

  def e9 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = futureMemo("only once", cache, futureDelay({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).runSequential must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e10 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = futureMemo("only once", cache, futureDelay({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).futureAttempt.runSequential must beRight(1).await
    invocationsNumber must be_==(1)
  }

  def e11 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = futureMemo("only once", cache, futureDelay({ invocationsNumber += 1; 1 }, timeout = Option(10000.millis)))

    (makeRequest >> makeRequest).runSequential must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e12 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    def makeRequest = futureMemo("only once", cache, futureDelay({ invocationsNumber += 1; 1 }, timeout = Option(10000.millis)))
    (makeRequest >> makeRequest).futureAttempt.runSequential must beRight(1).await

    invocationsNumber must be_==(1)
  }

  def e13 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    type S = Fx.fx2[Memoized, TimedFuture]
    def makeRequest = futureMemoized("only once", futureDelay[S, Int]({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).runFutureMemo(cache).runSequential must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e14 = {
    type S = Fx2[TimedFuture, Option]
    var lastActionDone = 0

    val action: Eff[S, Int] =
      for {
        i <- futureDelay[S, Int] { sleepFor(10.millis); 1 }
        _ <- futureFail[S, Int](new Exception("boom"))
        j =  i + 1
      } yield j

    val execute: Eff[S, Throwable Either Int] =
      action.
        addLast(futureDelay[S, Unit](lastActionDone += 1)).
        futureAttempt

    execute.runOption.runSequential must beSome(beLeft[Throwable]).awaitFor(20.seconds)
    lastActionDone must beEqualTo(1)
  }

  def e15 = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => futureDelay(i)).detachA(TimedFuture.ApplicativeTimedFuture)
    action.runNow(scheduler, ec) must be_===(list).awaitFor(10.second)
  }

  def retry1 = {
    type S = Fx2[TimedFuture, Option]
    var i = 0
    val before = System.currentTimeMillis

    val action: Eff[S, Int] =
      futureDelay[S, Int] { sleepFor(10.millis); i += 1; i }

    val durations = List(1.second, 1.second)

    val execute: Eff[S, Int] =
      action.retryUntil(i => i == 3, durations)

    execute.runOption.runSequential must beSome(3).awaitFor(10.seconds)

    val after = System.currentTimeMillis
    (after - before) must be_>(durations.map(_.toMillis).sum)
  }

  def retry2 = {
    type S = Fx2[TimedFuture, Option]
    var i = 0

    val action: Eff[S, Int] =
      futureDelay[S, Int] { sleepFor(10.millis); i += 1; i }

    val execute: Eff[S, Int] =
      action.retryUntil(i => i == 5, List(10.millis, 20.millis))

    execute.runOption.runSequential must beSome(3).await
  }

  /**
   * HELPERS
   */

  def boom: Unit = throw boomException
  val boomException: Throwable = new Exception("boom")

  def sleepFor(duration: FiniteDuration) =
    try Thread.sleep(duration.toMillis) catch { case t: Throwable => () }
}

