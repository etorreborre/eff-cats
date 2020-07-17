package org.atnos.eff

import Eff._
import cats._
import cats.implicits._

import scala.annotation.tailrec
import scala.util.Random

sealed trait Choose[T]
case class ChooseZero[T]() extends Choose[T]
case object ChoosePlus extends Choose[Boolean]

/**
 * The Choose effect models non-determinism
 * So we can get results, either:
 *   - no results (when using ChooseZero)
 *   - the result for action1 or the result for action b (when using ChoosePlus)
 *
 * When running this effect we can "collect" the results with any
 * F which has an Alternative instance.
 *
 * For example if F is List then:
 *  - no results is the empty list
 *  - the result for a or b is List(a, b)
 *
 * If F is Option then:
 *  - no results is the None
 *  - the result for a or b is Some(a) or Some(b
 */
trait ChooseEffect extends
  ChooseCreation with
  ChooseInterpretation

object ChooseEffect extends ChooseEffect

trait ChooseCreation {

  type _Choose[R] = Choose <= R
  type _choose[R] = Choose |= R

  def zero[R :_choose, A]: Eff[R, A] =
    send[Choose, R, A](ChooseZero[A]())

  def plus[R :_choose, A](a1: => Eff[R, A], a2: => Eff[R, A]): Eff[R, A] =
    EffMonad[R].flatMap(send(ChoosePlus))((b: Boolean) => if (b) a1 else a2)

  def chooseFrom[R :_choose, A](as: List[A]): Eff[R, A] =
    as match {
      case Nil => send[Choose, R, A](ChooseZero[A]())
      case a :: rest => plus(EffMonad[R].pure(a), chooseFrom(rest))
    }
}

object ChooseCreation extends ChooseCreation

trait ChooseInterpretation {

  def runChoose[R, U, A, F[_] : Alternative](r: Eff[R, A])(implicit m: Member.Aux[Choose, R, U]): Eff[U, F[A]] = {
    def lastRun(l: Last[R]): Last[U] =
      l match {
        case Last(None) => Last[U](None)
        case Last(Some(last)) => Last.eff(runChoose[R, U, Unit, F](last.value).as(()))
      }

    val alternativeF = Alternative[F]

    @tailrec
    def go(stack: List[Eff[R, A]], result: Eff[U, F[A]] = EffMonad[U].pure(alternativeF.empty), resultLast: Option[Last[U]] = None): Eff[U, F[A]] =
      stack match {
        case Nil =>
          resultLast match {
            case Some(last) => result.addLast(last)
            case None       => result
          }

        case e :: rest =>
          e match {
            case Pure(a, last) =>
              go(rest, (EffMonad[U].pure(alternativeF.pure(a)), result).mapN(alternativeF.combineK), resultLast.map(_ <* lastRun(last)))

            case Impure(NoEffect(a), c, last) =>
              runChoose(c(a).addLast(last))
              
            case Impure(u: Union[_, _], c, last) =>
              m.project(u) match {
                case Left(u1) =>
                  val r1 = Impure(u1, c.interpret(runChoose[R, U, A, F])(_.interpret(l => runChoose[R, U, Unit, F](l).void))).addLast(lastRun(last))
                  go(rest, (r1, result).mapN(alternativeF.combineK))

                case Right(choose) =>
                  choose match {
                    case ChooseZero() => go(rest, result)
                    case _ =>
                      val continuation = c.asInstanceOf[Continuation[R, Boolean, A]]
                      go(continuation(false) :: continuation(true) :: rest, result)
                  }
              }

            case ap @ ImpureAp(_, _, _) =>
              go(ap.toMonadic :: rest, result)
          }
      }

    go(List(r))
  }

}

object ChooseInterpretation extends ChooseInterpretation

trait ChooseImplicits {
  /**
   * MonadCombine implementation for the Eff[R, *] type if R contains the Choose effect
   */
  def EffMonadAlternative[R](implicit m: Member[Choose, R]): Alternative[Eff[R, *]] = new Alternative[Eff[R, *]] with Monad[Eff[R, *]] {
    def pure[A](a: A): Eff[R, A] =
      EffMonad[R].pure(a)

    def flatMap[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
      EffMonad[R].flatMap(fa)(f)

    def tailRecM[A, B](a: A)(f: A => Eff[R, Either[A, B]]): Eff[R, B] =
      flatMap(f(a)) {
        case Right(b)   => pure(b)
        case Left(next) => tailRecM(next)(f)
      }

    def empty[A]: Eff[R, A] =
      ChooseEffect.zero[R, A]

    def combineK[A](a1: Eff[R, A], a2: Eff[R, A]): Eff[R, A] =
      ChooseEffect.plus(a1, a2)
  }

}

object ChooseImplicits extends ChooseImplicits


/**
 * This class can be used as a F in runChoose
 * to generate random alternatives
 */
case class Rand[A](run: Random => Option[A])

object Rand {
  def none[A]: Rand[A] =
    Rand(_ => None)

  implicit def MonadCombineRandom: Alternative[Rand] = new Alternative[Rand] with Monad[Rand] {
    def pure[A](x: A): Rand[A] = Rand(_ => Option(x))

    def empty[A]: Rand[A] = Rand.none[A]

    def combineK[A](x: Rand[A], y: Rand[A]): Rand[A] =
      Rand { r =>
        if (r.nextBoolean()) {
          x.run(r) match {
            case Some(a) => Some(a)
            case None    => y.run(r)
          }
        } else y.run(r) match {
          case Some(a) => Some(a)
          case None    => x.run(r)
        }
      }

    def flatMap[A, B](fa: Rand[A])(f: (A) => Rand[B]): Rand[B] =
      Rand[B](rand => fa.run(rand).flatMap(f(_).run(rand)))

    def tailRecM[A, B](a: A)(f: (A) => Rand[Either[A, B]]): Rand[B] =
      Rand[B] { random =>
        Monad[Option].tailRecM(a)(f(_).run(random))
      }
  }

}
