package com.vwo.oldmonk.free_objects

import scalaz._
import Scalaz._

trait FreeMonoid[F[_]] extends Functor[F] {
  // A Free Monoid has a Monoid instance[P]
  implicit def monoid[A]: Monoid[F[A]]

  /* For any function f: A => B, B a boolean algebra, there exists a natural homomorphism
   * from the free boolean algebra to the algebra of B.
   * It satisfies the property that nat(f)(x.point) = f(x)
   */
  def nat[A,B](f: A => B)(implicit m: Monoid[B]): F[A] => B
}

object ListFreeMonoidInstance extends FreeMonoid[List] {
  implicit def monoid[A]: Monoid[List[A]] = listMonoid
  def map[A,B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  def nat[A,B](f: A=>B)(implicit m: Monoid[B]): List[A] => B = (l: List[A]) => l.map(f).reduce((x,y) => m.append(x,y))
}
