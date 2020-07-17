package org.atnos.site

object Cookbook extends UserGuidePage { def is = "Cookbook".title ^ s2"""

A collection of examples of how to solve particular problems with Eff

### Partial Interpretation

It's common to want to use different effects in different parts of a program. Some effects, like error handling or
logging, may extend through the whole of our program. However we may want to include additional effects,
like state, within one part.

The example below shows how we can do this

 - the `incrementNTimes` methods uses an additional `State` effect for its implementation (necessary to call `incrementCounter`)

 - the additional `State` effect is added to `R` with the `prepend` method: `Fx.prepend[StateInt, R]`

 - `runState` is called within `incrementNTimes` to finally interpret that effect

${snippet{
import cats._, data._
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

// Some type definitions for the effects we will use
type EitherString[A] = Either[String, A]
type WriterString[A] = Writer[String, A]
type StateInt[A]     = State[Int, A]

type _err[R] = EitherString |= R
type _log[R] = WriterString |= R
type _count[R] = StateInt |= R

/**
 * In module 1, some utility methods
 */

// repeat a side-effect n times
def repeatM[M[_] : Monad](n: Int, computation: M[Unit]): M[Unit] =
  if (n <= 0) computation
  else computation >> repeatM(n - 1, computation)

// check a condition and abort computations with a message if the condition is false
def assert[R :_err](condition: Boolean, msg: String): Eff[R, Unit] =
  if (!condition) left(msg) else right(())

// increment a counter and log the new value
def incrementCounter[R :_log :_count]: Eff[R, Unit] = for {
  c <- get
  c2 = c + 1
  _ <- tell(s"counter == $c2")
  _ <- put(c2)
} yield ()

/**
 * In module 2 your "business" logic
 */

// increment a value n times (n need to be positive)
def incrementNTimes[R :_err :_log](start: Int, times: Int): Eff[R, Int] = for {
  // this call uses the stack R
  _ <- assert(times >= 0, s"$times is negative")

  // the call uses the stack R plus an additional StateInt effect which is interpreted right away.
  // The other effects remain
  result <- repeatM(times, incrementCounter[Fx.prepend[StateInt, R]]()).execState(start)
} yield result

/**
 * A top-level call
 */

type Stack = Fx.fx2[EitherString, WriterString]

incrementNTimes[Stack](3, 2).runWriter.runEither.run
}.eval}

"""
}
