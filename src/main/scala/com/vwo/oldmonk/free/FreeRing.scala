package com.vwo.oldmonk.free

import scalaz._
import Scalaz._
import com.vwo.oldmonk.datastructures.CovariantSet
import spire.algebra.{Order => SpireOrder, _}
import spire.implicits._

trait FreeRingAlgebra[F[_]] extends Applicative[F] with Functor[F] with Monad[F] {
  // A Free Ring has a Ring[A]
  implicit def ring[A]: Ring[F[A]]

  /* For any function f: A => B, B a boolean algebra, there exists a natural homomorphism
   * from the free boolean algebra to the algebra of B.
   * It satisfies the property that nat(f)(x.point) = f(x)
   */
  def nat[A,B](f: A => B)(implicit ba: Ring[B]): F[A] => B

  override def map[A, B](fa: F[A])(f: A => B): F[B] = nat((a:A) => point[B](f(a)))(ring[B])(fa)
  def bind[A,B](fa: F[A])(f: A => F[B]): F[B] = nat((a:A) => f(a))(ring[B])(fa)
}

trait FreeRingInstances {
  case class FreeRingElem[A](e: Map[List[A], Int])

  implicit object FreeRingElemAlgebra extends FreeRingAlgebra[FreeRingElem] {
    implicit def ring[A] = new Ring[FreeRingElem[A]] {
      val zero = FreeRingElem[A](Map())
      val one = FreeRingElem[A](Map(List() -> 1))
      def negate(x: FreeRingElem[A]) = FreeRingElem[A](x.e.mapValues(v => -1*v))
      def plus(x: FreeRingElem[A], y: FreeRingElem[A]) = FreeRingElem[A](x.e + y.e)
      def times(x: FreeRingElem[A], y: FreeRingElem[A]) = {
        val combinations = for {
          xt <- x.e.toList
          yt <- y.e.toList
        } yield (xt._1 ++ yt._1, xt._2*yt._2)
        FreeRingElem[A](combinations.map(e => Map(e._1 -> e._2)).reduce( (x,y) => x + y))
      }
    }

    def point[A](x: =>A) = FreeRingElem[A](Map(List(x) -> 1))

    def nat[A,B](f: A => B)(implicit ba: Ring[B]): (FreeRingElem[A] => B) = (x:FreeRingElem[A]) => {
      val multiplied = x.e.toList.map(kv => {
        val kMultiplied = kv._1.foldLeft(ba.one)( (b: B, a: A) => ba.times(b,f(a)) )
        kv._2.toInt * kMultiplied
      })
      multiplied.reduce( ba.plus _ )
    }
  }
}
