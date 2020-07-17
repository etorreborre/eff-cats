package org.atnos.eff.addon.twitter

import cats._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.TimeoutException
import com.twitter.util._
import org.atnos.eff.concurrent.Scheduler

object TwitterFutureCreation extends TwitterFutureCreation

final case class TwitterTimedFuture[A](callback: (FuturePool, Scheduler) => Future[A], timeout: Option[FiniteDuration] = None) {
  @inline
  def runNow(pool: FuturePool, scheduler: Scheduler): Future[A] =
    timeout.fold(callback(pool, scheduler)) { t =>
      val promise = Promise[A]()
      val cancelTimeout = scheduler.schedule({ promise.updateIfEmpty(Throw(new TimeoutException)); () }, t)
      callback(pool, scheduler)
          .onFailure { e => cancelTimeout(); val _ = promise.updateIfEmpty(Throw(e)) }
          .onSuccess { a => cancelTimeout(); val _ = promise.updateIfEmpty(Return(a)) }
      promise

    }

}

object TwitterTimedFuture {

  final def ApplicativeTwitterTimedFuture: Applicative[TwitterTimedFuture] = new Applicative[TwitterTimedFuture] {
    def pure[A](x: A): TwitterTimedFuture[A] =
      TwitterTimedFuture((_, _) => Future.value(x))

    def ap[A, B](ff: TwitterTimedFuture[(A) => B])(fa: TwitterTimedFuture[A]): TwitterTimedFuture[B] = {
      val newCallback = { (pool: FuturePool, scheduler: Scheduler) =>
        val ffRan = ff.runNow(pool, scheduler)
        val faRan = fa.runNow(pool, scheduler)
        ffRan.joinWith(faRan)(_(_))
      }
      TwitterTimedFuture(newCallback)
    }

    override def toString = "Applicative[TwitterTimedFuture]"
  }

  implicit final val MonadTwitterTimedFuture: MonadError[TwitterTimedFuture, Throwable] = new MonadError[TwitterTimedFuture, Throwable] {
    def pure[A](x: A): TwitterTimedFuture[A] =
      TwitterTimedFuture((_, _) => Future.value(x))

    def flatMap[A, B](fa: TwitterTimedFuture[A])(f: (A) => TwitterTimedFuture[B]): TwitterTimedFuture[B] =
      TwitterTimedFuture[B]((pool, scheduler) => fa.runNow(pool, scheduler).flatMap(f(_).runNow(pool, scheduler)))

    def tailRecM[A, B](a: A)(f: (A) => TwitterTimedFuture[Either[A, B]]): TwitterTimedFuture[B] =
      TwitterTimedFuture[B]({ (pool, scheduler) =>
        def loop(va: A): Future[B] = f(va).runNow(pool, scheduler).flatMap {
          case Left(na) => loop(na)
          case Right(nb) => Future.value(nb)
        }
        loop(a)
      })

    def raiseError[A](e: Throwable): TwitterTimedFuture[A] =
      TwitterTimedFuture((p, s) => Future.exception(e))

    def handleErrorWith[A](fa: TwitterTimedFuture[A])(f: Throwable => TwitterTimedFuture[A]): TwitterTimedFuture[A] =
      TwitterTimedFuture((p, s) => fa.runNow(p, s).rescue[A] { case t => f(t).runNow(p, s) })

    override def toString = "MonadError[TwitterTimedFuture, Throwable]"
  }

  implicit val twitterFutureSequenceCached: SequenceCached[TwitterTimedFuture] =
    new SequenceCached[TwitterTimedFuture] {
      def get[X](cache: Cache, key: AnyRef): TwitterTimedFuture[Option[X]] =
        TwitterTimedFuture((pool, _) => pool(cache.get(key)))

      def apply[X](cache: Cache, key: AnyRef, sequenceKey: Int, tx: =>TwitterTimedFuture[X]): TwitterTimedFuture[X] =
        TwitterTimedFuture((pool, scheduler) => cache.memo((key, sequenceKey), tx.runNow(pool, scheduler)))

      def reset(cache: Cache, key: AnyRef): TwitterTimedFuture[Unit] =
        TwitterTimedFuture((_,_) => Future.value {
          cache.reset(key)
          var i = 0
          while (cache.get((key, i)).isDefined) {
            cache.reset((key, i))
            i += 1
          }
        })
    }

  def delay[A](a: =>A): TwitterTimedFuture[A] =
    TwitterTimedFuture((pool, scheduler) => Future.apply(a))

}

trait TwitterFutureTypes {
  type _future[R] = TwitterTimedFuture |= R
  type _Future[R] = TwitterTimedFuture <= R
}

trait TwitterFutureCreation extends TwitterFutureTypes {

  final def fromFutureWithExecutors[R :_future, A](c: (FuturePool, Scheduler) => Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TwitterTimedFuture, R, A](TwitterTimedFuture(c, timeout))

  final def fromFuture[R :_future, A](c: => Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TwitterTimedFuture, R, A](TwitterTimedFuture((_, _) => c, timeout))

  final def futureFail[R :_future, A](t: Throwable): Eff[R, A] =
    send[TwitterTimedFuture, R, A](TwitterTimedFuture((_, _) => Future.exception(t)))

  final def futureFromEither[R :_future, A](e: Throwable Either A): Eff[R, A] =
    e.fold(futureFail[R, A], Eff.pure[R, A])

  final def futureDelay[R :_future, A](a: => A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TwitterTimedFuture, R, A](TwitterTimedFuture((pool, _) => pool(a), timeout))

  final def futureFork[R :_future, A](a: => A)(pool: FuturePool, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TwitterTimedFuture, R, A](TwitterTimedFuture((_, _) => pool(a), timeout))

  def retryUntil[R :_future, A](e: Eff[R, A], condition: A => Boolean, durations: List[FiniteDuration]): Eff[R, A] =
    Eff.retryUntil(e, condition, durations, d => waitFor(d))

  def waitFor[R :_future](duration: FiniteDuration): Eff[R, Unit] =
    Eff.send(TwitterTimedFuture((_, scheduler) =>
      Future.Unit.delayed(Duration.fromNanoseconds(duration.toNanos))(twitterTimer)))

  val twitterTimer: JavaTimer =
    new JavaTimer()
}

trait TwitterFutureInterpretation extends TwitterFutureTypes {

  def runAsync[R, A](e: Eff[R, A])(implicit pool: FuturePool, scheduler: Scheduler, m: Member.Aux[TwitterTimedFuture, R, NoFx]): Future[A] =
    Eff.detachA(e)(TwitterTimedFuture.MonadTwitterTimedFuture, TwitterTimedFuture.ApplicativeTwitterTimedFuture, m).runNow(pool, scheduler)

  def runSequential[R, A](e: Eff[R, A])(implicit pool: FuturePool, scheduler: Scheduler, m: Member.Aux[TwitterTimedFuture, R, NoFx]): Future[A] =
    Eff.detach(e)(TwitterTimedFuture.MonadTwitterTimedFuture, m).runNow(pool, scheduler)

  import interpret.of

  final def futureAttempt[R, A](e: Eff[R, A])(implicit future: TwitterTimedFuture /= R): Eff[R, Throwable Either A] =
    interpret.interceptNatM[R, TwitterTimedFuture, Throwable Either *, A](e,
      new (TwitterTimedFuture ~> (TwitterTimedFuture of (Throwable Either *))#l) {
        override def apply[X](fa: TwitterTimedFuture[X]): TwitterTimedFuture[Throwable Either X] = attempt(fa)
      })

  final def attempt[A](a: TwitterTimedFuture[A]): TwitterTimedFuture[Throwable Either A] = {
    TwitterTimedFuture[Throwable Either A](callback =
      a.runNow(_, _).liftToTry.map {
        case Throw(ex) => Either.left(ex)
        case Return(v) => Either.right(v)
      }
    )
  }

  /** memoize future result using a cache */
  final def memoize[A](key: AnyRef, cache: Cache, future: TwitterTimedFuture[A]): TwitterTimedFuture[A] =
    TwitterTimedFuture { (pool, scheduler) =>
      val promise = Promise[A]()

      cache.get[A](key).fold {
        future.runNow(pool, scheduler).map { v => val _ = cache.put(key, v); v }.proxyTo(promise)
      } { v => promise.setValue(v) }

      promise
    }

  /**
   * Memoize future effects using a cache
   *
   * if this method is called with the same key the previous value will be returned
   */
  final def futureMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(implicit future: TwitterTimedFuture /= R): Eff[R, A] =
    futureAttempt(Eff.memoizeEffect(e, cache, key)).flatMap {
      case Left(t)  => Eff.send(TwitterTimedFuture.twitterFutureSequenceCached.reset(cache, key)) >> TwitterFutureEffect.futureFail(t)
      case Right(a) => Eff.pure(a)
    }

  /**
   * Memoize Future values using a memoization effect
   *
   * if this method is called with the same key the previous value will be returned
   */
  final def futureMemoized[R, A](key: AnyRef, e: Eff[R, A])(implicit future: TwitterTimedFuture /= R, m: Memoized |= R): Eff[R, A] =
    MemoEffect.getCache[R].flatMap(cache => futureMemo(key, cache, e))

  def runFutureMemo[R, U, A](cache: Cache)(effect: Eff[R, A])(implicit m: Member.Aux[Memoized, R, U], task: TwitterTimedFuture |= U): Eff[U, A] = {
    interpret.translate(effect)(new Translate[Memoized, U] {
      def apply[X](mx: Memoized[X]): Eff[U, X] =
        mx match {
          case Store(key, value) => TwitterFutureEffect.futureDelay(cache.memo(key, value()))
          case GetCache()        => TwitterFutureEffect.futureDelay(cache)
        }
    })
  }
}

object TwitterFutureInterpretation extends TwitterFutureInterpretation

trait TwitterFutureEffect extends TwitterFutureCreation with TwitterFutureInterpretation

object TwitterFutureEffect extends TwitterFutureEffect
