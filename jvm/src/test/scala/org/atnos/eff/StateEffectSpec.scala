package org.atnos.eff

import org.specs2.{ScalaCheck, Specification}
import cats.data._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

class StateEffectSpec extends Specification with ScalaCheck { def is = s2"""

 The state monad can be used to put/get state $putGetState
 modify can be used to modify the current state $modifyState
 The state monad works applicatively $applicativeState

 A State[T, *] effect can be lifted into a State[S, *] effect
   provided there is a "lens" (a getter and a setter) from T to S $lensedState
   with an implicit conversion                                    $stateImplicitLens

   Also from a stack to another
   provided there is a "lens" and an IntoPoly instance from one stack to another $stateIntoAnother

 The Eff monad is stack safe with State $stacksafeState

"""

  def putGetState = {

    def action[R:_stateInt]: Eff[R, String] = for {
      a <- get
      h <- EffMonad.pure("hello")
      _ <- put(a + 5)
      b <- get
      _ <- put(b + 10)
      w <- EffMonad.pure("world")
    } yield h+" "+w

    action[SI].runState(5).run ==== (("hello world", 20))
  }

  def modifyState = {

    def action[R :_stateInt]: Eff[R, String] = for {
       a <- get
       _ <- put(a + 1)
       _ <- modify((_:Int) + 10)
    } yield a.toString

    action[SI].execStateZero[Int].run ==== 11
  }

  def stacksafeState = {

    val list = (1 to 10000).toList
    val action = list.traverse(i => StateEffect.put[SI, Int](i).as(i.toString))

    action.runState(0).run ==== ((list.map(_.toString), list.size))
  }

  def applicativeState = {

    val stateAction = StateEffect.modify[SI, Int](_ + 1)

    (stateAction *> stateAction).runState(0).run ====
      (stateAction >> stateAction).runState(0).run
  }


  def lensedState = {
    type StateIntPair[A] = State[(Int, Int), A]
    type TS = Fx.fx2[StateInt, Option]

    val action: Eff[TS, String] =
      for {
        _ <- put[TS, Int](10)
        h <- OptionEffect.some[TS, String]("hello")
        _ <- modify[TS, Int](_ + 2)
      } yield h

    val getter = (pair: (Int, Int)) => pair._2
    val setter = (pair: (Int, Int), j: Int) => (pair._1, j)

    val lensed = action.lensState(getter, setter)

    lensed.runOption.runState((20, 30)).run ==== ((Some("hello"), (20, 12)))
  }

  def stateIntoAnother = {
    type StateIntPair[A] = State[(Int, Int), A]
    type TS = Fx.fx2[StateInt, Option]
    type BR = Fx.fx3[StateIntPair, Option, Writer[String, *]]

    val action: Eff[TS, String] =
      for {
        _ <- put[TS, Int](10)
        h <- OptionEffect.some[TS, String]("hello")
        _ <- modify[TS, Int](_ + 2)
      } yield h

    val getter = (pair: (Int, Int)) => pair._2
    val setter = (pair: (Int, Int), j: Int) => (pair._1, j)

    val lensed: Eff[BR, String] = action.intoState(getter, setter)

    lensed.runOption.runWriterNoLog.runState((20, 30)).run ==== ((Some("hello"), (20, 12)))
  }

  def stateImplicitLens = {
    import state._

    case class Address(s: String)
    case class Person(address: Address)

    implicit val getAddress: Person => Address = (p: Person) => p.address
    implicit val setAddress: Address => Person => Person = (a: Address) => (p: Person) => p.copy(address = a)

    type PerS[E] = State[Person, *] |= E
    type PerR[E] = Reader[Person, *] |= E
    type Add[E] = State[Address, *] |= E
    type Err[E] = Either[String, *] |= E

    def isBadPerson[E: PerR: Err]: Eff[E, Boolean] =
      ask.map(_.hashCode % 13 == 0)

    def updateAddress[E: Add: Err]: Eff[E, Unit] =
      get.void

    def updatePerson[E: PerS: Err]: Eff[E, Unit] =
      for {
        bad <- isBadPerson()
        _   <- updateAddress()
      } yield ()

    updatePerson[Fx.fx2[String Either *, State[Person, *]]]().evalState(Person(Address("here"))).runEither.run ==== Right(())
  }

  type StateInt[A] = State[Int, A]

  type SI = Fx.fx1[StateInt]
  type _stateInt[R] = StateInt |= R

}
