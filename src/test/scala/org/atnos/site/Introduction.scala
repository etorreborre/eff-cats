package org.atnos.site

import cats.data._
import cats.Eval
import org.atnos.eff._

object Introduction extends UserGuidePage { def is = "Introduction".title ^ s2"""

Extensible effects are an alternative to monad transformers for computing with effects in a functional way.
This library uses a "free-er" monad and extensible effects to create an "effect stack" as described in
[Oleg Kiselyov's paper](http://okmij.org/ftp/Haskell/extensible/more.pdf).

There are lots of advantages to this approach:

 - operations can be declared as "requiring" an effect, without the need to fix the full stack of effects in advance

 - effects handlers are modular and can be replaced with other implementations as needed (even at runtime)

 - the underlying implementation is performant and stack-safe

 - existing monadic datatypes can be integrated to the library

 - effect stacks can be modified or combined

This is probably very abstract so let's see more precisely what this all means.

### First example

*Please make sure you have done all the setup described on the ${Installation} page first*

A monadic action is modelled as a value of type `Eff[R, A]` where `R` denotes a set of effects and `A` is the value
returned by the computation, possibly triggering some effects when evaluated.

The effects `R` are modelled by a type-level list of "effect constructors", for example:${snippet{
import cats._, data._
import org.atnos.eff._

type ReaderInt[A] = Reader[Int, A]
type WriterString[A] = Writer[String, A]

type Stack = Fx.fx3[WriterString, ReaderInt, Eval]

}}
The stack `Stack` above declares 3 effects:

 - a `ReaderInt` effect to access some configuration number of type `Int`

 - a `WriterString` effect to log string messages

 - an `Eval` effect to only compute values on demand (a bit like lazy values)

Now we can write a program with those 3 effects, using the primitive operations provided by `ReaderEffect`, `WriterEffect` and `EvalEffect`:${snippet{
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

// useful type aliases showing that the ReaderInt and the WriterString effects are "members" of R
// note that R could have more effects
type _readerInt[R]    = ReaderInt |= R
type _writerString[R] = WriterString |= R

def program[R :_readerInt :_writerString :_eval]: Eff[R, Int] = for {
  // get the configuration
  n <- ask[R, Int]

  // log the current configuration value
  _ <- tell("the required power is "+n)

  // compute the nth power of 2
  a <- delay(math.pow(2, n.toDouble).toInt)

  // log the result
  _ <- tell("the result is "+a)
} yield a

// run the action with all the interpreters
// each interpreter running one effect
program[Stack]().runReader(6).runWriter.runEval.run
}.eval}

As you can see, all the effects of the `Stack` type are being executed one by one:

 1. the `Reader` effect, which provides a value, `6`, to each computation needing it
 2. the `Writer` effect, which logs messages
 3. the `Eval` effect to compute the "power of 2 computation"
 4. `run` extracts the final result

<br/>
Maybe you noticed that the effects are not being executed in the same order as their order in the stack declaration.
The effects can indeed be executed in any order. This doesn't mean though that the results will be the same. For example
running the `Writer` effect then `Either` effect returns `String Either (A, List[String])` whereas running the `Either` effect
 then the `Writer` effect returns `(String Either A, List[String])`.

This all works thanks to some implicits definitions guiding Scala type inference towards the
right return types. You can learn more on implicits in the ${"implicits" ~/ MemberImplicits} section.

You can now get a more detailed presentation of the use of the Eff monad by reading the ${"tutorial" ~/ Tutorial} or
you can learn about ${"other effects" ~/ OutOfTheBox} supported by this library.
"""

  type ReaderInt[A] = Reader[Int, A]
  type WriterString[A] = Writer[String, A]

  type Stack = Fx.fx3[WriterString, ReaderInt, Eval]


}

