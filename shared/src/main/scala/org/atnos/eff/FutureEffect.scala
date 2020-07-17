package org.atnos.eff

import java.util.concurrent.TimeoutException

import cats._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.concurrent.Scheduler

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

object FutureCreation extends FutureCreation

final case class TimedFuture[A](callback: (Scheduler, ExecutionContext) => Future[A], timeout: Option[FiniteDuration] = None) {
  @inline def runNow(scheduler: Scheduler, ec: ExecutionContext): Future[A] =
    timeout.fold(callback(scheduler, ec)) { t =>
      val promise = Promise[A]()
      val cancelTimeout = scheduler.schedule({ promise.tryFailure(new TimeoutException); () }, t)
      promise.completeWith(callback(scheduler, ec).map(a => { cancelTimeout(); a })(ec))
      promise.future
    }
}

object TimedFuture {

  final def ApplicativeTimedFuture: Applicative[TimedFuture] = new Applicative[TimedFuture] {
    def pure[A](x: A): TimedFuture[A] =
      TimedFuture((_, _) => Future.successful(x))

    def ap[A, B](ff: TimedFuture[(A) => B])(fa: TimedFuture[A]): TimedFuture[B] = {
      val newCallback = { (scheduler: Scheduler, ec: ExecutionContext) =>
        val ffRan = ff.runNow(scheduler, ec)
        val faRan = fa.runNow(scheduler, ec)
        faRan.flatMap(a => ffRan.map(f => f(a))(ec))(ec)
      }
      TimedFuture(newCallback)
    }

    override def toString = "Applicative[TimedFuture]"
  }

  implicit final def MonadTimedFuture: MonadError[TimedFuture, Throwable] = new MonadError[TimedFuture, Throwable] {
    def pure[A](x: A): TimedFuture[A] =
      TimedFuture((_, _) => Future.successful(x))

    def flatMap[A, B](fa: TimedFuture[A])(f: (A) => TimedFuture[B]): TimedFuture[B] =
      TimedFuture[B]((scheduler, ec) => fa.runNow(scheduler, ec).flatMap(f(_).runNow(scheduler, ec))(ec))

    def tailRecM[A, B](a: A)(f: (A) => TimedFuture[Either[A, B]]): TimedFuture[B] =
      TimedFuture[B]({ (scheduler, ec) =>
        def loop(va: A): Future[B] = f(va).runNow(scheduler, ec).flatMap {
          case Left(na) => loop(na)
          case Right(nb) => Future.successful(nb)
        }(ec)
        loop(a)
      })

    def raiseError[A](e: Throwable): TimedFuture[A] =
      TimedFuture((s, ec) => Future.failed(e))

    def handleErrorWith[A](fa: TimedFuture[A])(f: Throwable => TimedFuture[A]): TimedFuture[A] =
      TimedFuture((s, ec) => fa.runNow(s, ec).recoverWith[A] { case t => f(t).runNow(s, ec) }(ec))

    override def toString = "MonadError[TimedFuture, Throwable]"
  }
}

trait FutureTypes {
  type _future[R] = TimedFuture |= R
  type _Future[R] = TimedFuture <= R
}

trait FutureCreation extends FutureTypes {

  final def fromFutureWithExecutors[R :_future, A](c: (Scheduler, ExecutionContext) => Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture(c, timeout))

  final def fromFuture[R :_future, A](c: => Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture((_, _) => c, timeout))

  final def futureFail[R :_future, A](t: Throwable): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture((_, _) => Future.failed(t)))

  final def futureFromEither[R :_future, A](e: Throwable Either A): Eff[R, A] =
    e.fold(futureFail[R, A], Eff.pure[R, A])

  final def futureDelay[R :_future, A](a: => A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture((_, ec) => Future(a)(ec), timeout))

  final def futureFork[R :_future, A](a: => A, ec: ExecutionContext, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture((_, _) => Future(a)(ec), timeout))

  final def futureDefer[R :_future, A](a: => Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture((_, _) => a, timeout))

  def retryUntil[R :_future, A](e: Eff[R, A], condition: A => Boolean, durations: List[FiniteDuration]): Eff[R, A] =
    Eff.retryUntil(e, condition, durations, d => waitFor(d))

  def waitFor[R :_future](duration: FiniteDuration): Eff[R, Unit] =
    Eff.send(TimedFuture((scheduler, _) => scheduler.delay(duration)))
}

trait FutureInterpretation extends FutureTypes {

  def runAsyncOn[R, A](executorServices: ExecutorServices)(e: Eff[R, A])(implicit m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
    runAsync(e)(executorServices.scheduler, executorServices.executionContext, m)

  def runAsync[R, A](e: Eff[R, A])(implicit scheduler: Scheduler, exc: ExecutionContext, m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
    Eff.detachA(Eff.effInto[R, Fx1[TimedFuture], A](e))(TimedFuture.MonadTimedFuture, TimedFuture.ApplicativeTimedFuture).runNow(scheduler, exc)

  def runSequentialOn[R, A](executorServices: ExecutorServices)(e: Eff[R, A])(implicit m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
    runSequential(e)(executorServices.scheduler, executorServices.executionContext, m)

  def runSequential[R, A](e: Eff[R, A])(implicit scheduler: Scheduler, exc: ExecutionContext, m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
    Eff.detach(Eff.effInto[R, Fx1[TimedFuture], A](e)).runNow(scheduler, exc)

  import interpret.of

  final def futureAttempt[R, A](e: Eff[R, A])(implicit future: TimedFuture /= R): Eff[R, Throwable Either A] =
    interpret.interceptNatM[R, TimedFuture, Throwable Either *, A](e,
      new (TimedFuture ~> (TimedFuture of (Throwable Either *))#l) {
        override def apply[X](fa: TimedFuture[X]): TimedFuture[Throwable Either X] = attempt(fa)
      })

  final def attempt[A](a: TimedFuture[A]): TimedFuture[Throwable Either A] = {
    TimedFuture[Throwable Either A](callback = (scheduler, ec) => {
      val prom = Promise[Throwable Either A]()
      a.runNow(scheduler, ec).onComplete { t =>
        prom.success(t match {
          case Failure(ex) => Either.left(ex)
          case Success(v) => Either.right(v)
        })
      }(ec)
      prom.future
    })
  }

  final def memoize[A](key: AnyRef, cache: Cache, future: TimedFuture[A]): TimedFuture[A] =
    TimedFuture { (scheduler, ec) =>
      val prom = Promise[A]()
      cache.get[A](key).fold {
        prom.completeWith(future.runNow(scheduler, ec).map { v => val _ = cache.put(key, v); v }(ec))
      } { v => prom.success(v) }
      prom.future
    }

  /**
    * Memoize future values using a cache
    *
    * if this method is called with the same key the previous value will be returned
    */
  final def futureMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(implicit future: TimedFuture /= R): Eff[R, A] =
    interpret.interceptNat[R, TimedFuture, A](e)(
      new (TimedFuture ~> TimedFuture) {
        override def apply[X](fa: TimedFuture[X]): TimedFuture[X] = memoize(key, cache, fa)
      }
    )

  /**
    * Memoize Future values using a memoization effect
    *
    * if this method is called with the same key the previous value will be returned
    */
  final def futureMemoized[R, A](key: AnyRef, e: Eff[R, A])(implicit future: TimedFuture /= R, m: Memoized |= R): Eff[R, A] =
    MemoEffect.getCache[R].flatMap(cache => futureMemo(key, cache, e))

}

object FutureInterpretation extends FutureInterpretation

trait FutureEffect extends FutureCreation with FutureInterpretation

object FutureEffect extends FutureEffect
