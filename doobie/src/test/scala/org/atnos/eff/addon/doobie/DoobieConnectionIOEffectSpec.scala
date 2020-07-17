package org.atnos.eff.addon.doobie

import org.specs2.Specification
import doobie._, doobie.implicits._
import cats.effect._
import org.atnos.eff.{Eff, Fx}
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.addon.doobie._
import org.specs2.matcher.{ThrownExpectations, ValueCheck}
import doobie.free.connection


class DoobieConnectionIOEffectSpec extends Specification with ThrownExpectations { def is = sequential ^ s2"""
  ConnectionIO effects can be converted to IO-like effects $t1

  Failures are properly handled
    in programs                 $t2
    in strategy.before          $t3
    in strategy.after           $t4
    in strategy.oops            $t5
    in strategy.always          $t6
"""

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContexts.synchronous)

  type Stack = Fx.fx2[ConnectionIO, IO]

  def t1 = {
    val p: Eff[Stack, Int] = for {
      a <- IO(1).send[Stack]
      b <- queryTable(2).send[Stack]
      c <- IO(3).send[Stack]
      d <- queryTable(4).send[Stack]
    } yield a + b + c + d

    val (xa, c) = H2TestableTransactor.create[IO]()

    p.runConnectionIO(xa).detach.unsafeRunSync() must_== 10
    c.calls must_== List("connection", "before", "after", "always")
  }

  def t2 = {
    val p: Eff[Stack, Int] = for {
      a <- IO(1).send[Stack]
      b <- queryTable(2).send[Stack]
      c <- IO[Int](throw new Error("c failed")).send[Stack]
    } yield a + b + c

    val (xa, c) = H2TestableTransactor.create[IO]()

    p.runConnection(xa) must beLeft(withExceptionMessage("c failed"))
    c.calls must_== List("connection", "before", "oops", "always")
  }

  def t3 = {
    val properProgram: Eff[Stack, Int] = for {
      a <- IO(1).send[Stack]
      b <- queryTable(2).send[Stack]
    } yield a + b

    val (xa, c) = H2TestableTransactor.create[IO](before = connection.delay(throw new Error("before failed")))

    properProgram.runConnection(xa) must beLeft(withExceptionMessage("before failed"))
    c.calls must_== List("connection", "oops", "always")
  }

  def t4 = {
    val properProgram: Eff[Stack, Int] = for {
      a <- IO(1).send[Stack]
      b <- queryTable(2).send[Stack]
    } yield a + b

    val (xa, c) = H2TestableTransactor.create[IO](after = connection.delay(throw new Error("after failed")))

    properProgram.runConnection(xa) must beLeft(withExceptionMessage("after failed"))
    c.calls must_== List("connection", "before", "oops", "always")
  }

  def t5 = {
    val erroneousProgram: Eff[Stack, Int] =
      IO[Int](throw new Error("program failed")).send[Stack]

    val (xa, c) = H2TestableTransactor.create[IO](oops = connection.delay(throw new Error("oops failed")))

    erroneousProgram.runConnection(xa) must beLeft(withExceptionMessage("oops failed"))
    c.calls must_== List("connection", "before", "always")
  }

  def t6 = {
    val properProgram: Eff[Stack, Int] = IO(1).send[Stack]

    val (xa, c) = H2TestableTransactor.create[IO](always = connection.delay(throw new Error("always failed")))

    properProgram.runConnection(xa) must beLeft(withExceptionMessage("always failed"))
    c.calls must_== List("connection", "before", "after")
  }

  /**
   * HELPERS
   */
  
  implicit class RunConnectionOps[A](e: Eff[Stack, A]) {
    def runConnection(xa: Transactor[IO]): Throwable Either A =
      e.runConnectionIO(xa).detach.attempt.unsafeRunSync()
  }

  def queryTable(i: Int): ConnectionIO[Int] = sql"select $i".query[Int].unique

  def withExceptionMessage(msg: String): ValueCheck[Throwable] = { err: Throwable =>
    err.getMessage must_== msg
  }
}
