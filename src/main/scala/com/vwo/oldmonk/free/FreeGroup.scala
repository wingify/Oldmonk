package com.vwo.oldmonk.free

import scalaz._
import Scalaz._

import spire.algebra.{Order => SpireOrder, _}
import spire.implicits._

trait FreeGroup[F[_]] extends Functor[F] {
  // A Free Monoid has a Monoid instance[P]
  implicit def group[A]: Group[F[A]]
  def point[A](a: A): F[A]

  /* For any function f: A => B, B a boolean algebra, there exists a natural homomorphism
   * from the free boolean algebra to the algebra of B.
   * It satisfies the property that nat(f)(x.point) = f(x)
   */
  def nat[A,B](f: A => B)(implicit m: Group[B]): F[A] => B

  //This is probably not the most efficient way, but it's so pretty
  def map[A,B](fa: F[A])(f: A => B) = nat((a:A) => point[B](f(a)))(group[B])(fa)
}

sealed trait FreeGroupElement[+A]
case class Obj[+A](o: A) extends FreeGroupElement[A]
case class Inv[+A](o: A) extends FreeGroupElement[A]

case class FreeGroupWord[+A](word: List[FreeGroupElement[A]])

object FreeGroupWordInstance extends FreeGroup[FreeGroupWord] {
  def point[A](a: A): FreeGroupWord[A] = FreeGroupWord(List(Obj(a)))

  def group[A] = new Group[FreeGroupWord[A]] {
    lazy val id = FreeGroupWord[A](List())

    def inverse(a: FreeGroupWord[A]) = FreeGroupWord(a.word.reverse.map(x => x match {
      case Obj(o) => Inv(o)
      case Inv(o) => Obj(o)
    }))

    private def inverseOf(x: FreeGroupElement[A], y: FreeGroupElement[A]): Boolean = (x,y) match {
      case (Obj(t1), Inv(t2)) if (t1 == t2) => true
      case (Inv(t1), Obj(t2)) if (t1 == t2) => true
      case _ => false
    }

    def op(a: FreeGroupWord[A], b: FreeGroupWord[A]): FreeGroupWord[A] = {
      var aRev = a.word.reverse
      var bit = b.word
      var finished = aRev.isEmpty || bit.isEmpty || inverseOf(aRev.head, bit.head)
      while (!finished) {
        aRev = aRev.tail
        bit = bit.tail
        finished = aRev.isEmpty || bit.isEmpty || inverseOf(aRev.head, bit.head)
      }
      FreeGroupWord(aRev.reverse ++ bit)
    }
  }
  def point[A](a: =>A) = FreeGroupWord[A](List(Obj(a)))
  def nat[A,B](f: A => B)(implicit g: Group[B]) = (x:FreeGroupWord[A]) => {
    val word = x.word
    val bElem: List[B] = word.map((x:FreeGroupElement[A]) => x match {
      case Obj(a) => f(a)
      case Inv(a) => g.inverse(f(a))
    })
    bElem.reduce(g.op _)
  }

}
