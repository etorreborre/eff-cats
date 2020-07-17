package org.atnos.eff

import org.specs2.{ScalaCheck, Specification}
import org.scalacheck.Gen.posNum
import cats.syntax.all._
import cats.instances.all._
import cats.data._
import Eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

class OptionEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the option monad                     $optionMonad
 run the option monad with nothing        $optionWithNothingMonad
 run the option monad with reader         $optionReader

 The Eff monad is stack safe with Option  $stacksafeOption

"""

  def optionMonad = {
    type S = Fx.fx1[Option]

    val option: Eff[S, String] =
      for {
        s1 <- OptionEffect.some("hello")
        s2 <- OptionEffect.some("world")
      } yield s1 + " " + s2

    option.runOption.run === Some("hello world")
  }

  def optionWithNothingMonad = {
    type S = Fx.fx1[Option]

    val option: Eff[S, String] =
      for {
        s1 <- OptionEffect.some[S, String]("hello")
        s2 <- OptionEffect.none[S, String]
      } yield s1 + " " + s2

    option.runOption.run === None
  }

  def optionReader = prop { (init: Int, someValue: Int) =>

    // define a Reader / Option stack
    type S = Fx.fx2[Option, ReaderInt]

    // create actions
    def readOption[R :_option :_readerInt]: Eff[R, Int] =
      for {
        j <- OptionEffect.some(someValue)
        i <- ask
      } yield i + j

    // run effects
    readOption[S]().runOption.runReader(init).run must_== Some(init + someValue)
  }.setGens(posNum[Int], posNum[Int]).set(minTestsOk = 1)


  def stacksafeOption = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => OptionEffect.some(i))

    action.runOption.run ==== Some(list)
  }

  type ReaderInt[A] = Reader[Int, A]

  type _readerInt[R] = ReaderInt |= R
}
