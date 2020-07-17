package org.atnos.site
package lib

import org.specs2.matcher.ExpectationsDescription._

object TimedFutureEffectPage extends UserGuidePage { def is = "TimedFuture".title ^ s2"""

The `TimedFuture` effect is a thin shim on top of Scala's `Future`. The only extra capability it has built-in is
timeouts, which are supported by passing in a `ScheduledExecutionContext`.
Note that because `Future` represents a computation already taking place, `TimedFuture` is a function that returns a `Future`.
This means if you start a computation before passing the `Future` into Eff, the `Future` will begin less predictably.

Now, let's create some `TimedFuture` effects:${snippet{

import org.atnos.eff._
import org.atnos.eff.future._
import org.atnos.eff.syntax.all._

import scala.concurrent._, duration._
import scala.concurrent.ExecutionContext.Implicits.global

type R = Fx.fx2[TimedFuture, Option]

val action: Eff[R, Int] =
  for {
  // create a value now
    a <- Eff.pure[R, Int](1)

    // evaluate a value later, on some other thread pool, and continue when it's finished
    b <- futureDelay[R, Int](1)
  } yield b

/*p
Then we need to pass a `Scheduler` and an `ExecutionContext` in to begin the computation.
*/

implicit val scheduler = ExecutorServices.schedulerFromGlobalExecutionContext
import org.atnos.eff.syntax.future._

Await.result(action.runOption.runSequential, 1 second)
}.eval}

You can also use other `Future` or `Task` effects:

 - `twitter`: depend on `eff-twitter` and import `org.atnos.eff.addon.twitter.future._`
 - `scalaz`: depend on `eff-scalaz` and import `org.atnos.eff.addon.scalaz.task._`
 - `monix`: depend on `eff-monix` and import `org.atnos.eff.addon.monix.task._`

There are corresponding syntax imports to be able to call `runAsync` methods in:

 - `twitter`: `org.atnos.eff.syntax.addon.twitter.future._`
 - `scalaz`: `org.atnos.eff.syntax.addon.scalaz.task._`
 - `monix`: `org.atnos.eff.syntax.addon.monix.task._`

`Future` and `Task` computations can also be memoized to avoid expensive computations to be done several times. You can either

 - use the `futureMemo/taskMemo` operator with a (mutable) cache
 - use the `futureMemoized/taskMemoized` operator with the `Memoized` effect (you will need to provide the cache later)
<p/>

${snippet{
import cats.implicits._
import org.atnos.eff._, future._, all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.future._
import scala.concurrent._, duration._
import scala.concurrent.ExecutionContext.Implicits.global

var i = 0

def expensive[R :_Future :_memo]: Eff[R, Int] =
  futureMemoized[R, Int]("key", futureDelay[R, Int] { i += 1; 10 * 10})

type S = Fx.fx2[Memoized, TimedFuture]

implicit val scheduler = ExecutorServices.schedulerFromGlobalExecutionContext

val futureMemo: Future[Int] =
  (expensive[S]() >> expensive[S]()).runFutureMemo(ConcurrentHashMapCache()).runSequential

Await.result(futureMemo, 1.second)

"there is only one invocation" <==> (i === 1)

}.eval}


"""
}
