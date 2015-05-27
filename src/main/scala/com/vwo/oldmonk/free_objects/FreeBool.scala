package com.vwo.oldmonk.free_objects

import spire.algebra.{Order => SpireOrder, _}
import scalaz._
import Scalaz._
import com.vwo.oldmonk.datastructures.CovariantSet

trait FreeBoolAlgebra[F[_]] {
  // A Free Boolean Algebra has a Bool[P]
  def bool[A]: Bool[F[A]]

  /* For any function f: A => B, B a boolean algebra, there exists a natural homomorphism
   * from the free boolean algebra to the algebra of B.
   * It satisfies the property that nat(f)(x.point) = f(x)
   */
  def nat[A,B](f: A => B)(implicit ba: Bool[B]): F[A] => B

  def concrete[A]: ConcreteFreeBoolAlgebra[A,F] = new ConcreteFreeBoolAlgebra[A,F] {
    val bool = FreeBoolAlgebra.this.bool[A]
    def nat[B](f: A=>B)(implicit ba: Bool[B]) = FreeBoolAlgebra.this.nat[A,B](f)
  }
}

trait ConcreteFreeBoolAlgebra[A, F[_]] {
  // A Free Boolean Algebra has a Bool[P]
  def bool: Bool[F[A]]

  /* For any function f: A => B, B a boolean algebra, there exists a natural homomorphism
   * from the free boolean algebra to the algebra of B.
   * It satisfies the property that nat(f)(x.point) = f(x)
   */
  def nat[B](f: A => B)(implicit ba: Bool[B]): F[A] => B
}

trait FreeBoolSyntax {
  def truePred[P, F[_]](implicit a: FreeBoolAlgebra[F]): F[P] = a.bool.one
  def falsePred[P, F[_]](implicit a: FreeBoolAlgebra[F]): F[P] = a.bool.zero
}

trait FreeBoolListInstances {
  // The simplest freebool instance - it doesn't make any attempt to simplify the objects.

  sealed trait FreeBoolList[+P]

  object FreeBoolList {
    sealed trait ConstantFreeBoolList extends FreeBoolList[Nothing]
    case object TruePred extends ConstantFreeBoolList
    case object FalsePred extends ConstantFreeBoolList

    case class Pred[+P](p: P) extends FreeBoolList[P]
    case class Negate[+P](term: FreeBoolList[P]) extends FreeBoolList[P]
    case class AndPred[+P](terms: List[FreeBoolList[P]]) extends FreeBoolList[P]
    case class OrPred[+P](terms: List[FreeBoolList[P]]) extends FreeBoolList[P]
  }
  import FreeBoolList._

  object FreeBoolListAlgebra extends FreeBoolAlgebra[FreeBoolList] {
    def bool[P] = new Bool[FreeBoolList[P]] {
      def and(a: FreeBoolList[P], b: FreeBoolList[P]) = (a,b) match {
        case (FalsePred, _) => FalsePred
        case (_, FalsePred) => FalsePred
        case (TruePred, x:AndPred[P]) => x
        case (x: AndPred[P], TruePred) => x
        case (AndPred(terms1), AndPred(terms2)) => AndPred(terms1 ++ terms2)
        case (AndPred(terms), x) => AndPred(x :: terms)
        case (x, AndPred(terms)) => AndPred(x :: terms)
        case (x,y) => AndPred(List(x,y))
      }
      def or(a: FreeBoolList[P], b: FreeBoolList[P]) = (a,b) match {
        case (TruePred, _) => TruePred
        case (_, TruePred) => TruePred
        case (FalsePred, x:AndPred[P]) => x
        case (x: OrPred[P], FalsePred) => x
        case (OrPred(terms1), OrPred(terms2)) => OrPred(terms1 ++ terms2)
        case (OrPred(terms), x) => OrPred(x :: terms)
        case (x, OrPred(terms)) => OrPred(x :: terms)
        case (x,y) => OrPred(List(x,y))
      }
      def complement(a: FreeBoolList[P]) = a match {
        case Negate(x) => x
        case FalsePred => TruePred
        case TruePred => FalsePred
        case x => Negate(x)
      }
      def zero: FreeBoolList[P] = FalsePred: FreeBoolList[P]
      def one: FreeBoolList[P] = TruePred
    }

    def nat[A,B](f: A => B)(implicit ba: Bool[B]): (FreeBoolList[A] => B) = {
      def homo(a: FreeBoolList[A]): B = a match {
        case Pred(x) => f(x)
        case Negate(x) => ba.complement(homo(x))
        case AndPred(terms) => terms.foldLeft(ba.one)( (x:B,y:FreeBoolList[A]) => ba.and(x, homo(y)) )
        case OrPred(terms) => terms.foldLeft(ba.zero)( (x:B,y:FreeBoolList[A]) => ba.or(x, homo(y)) )
        case TruePred => ba.one
        case FalsePred => ba.zero
      }
      homo
    }
  }
}

trait FreeBoolSimpleInstances {
  sealed trait FreeBoolSimple[+P]

  case object FreeBoolSimple {
    sealed trait NotAndPred
    sealed trait NotOrPred

    // True and false
    sealed trait ConstantFreeBoolSimple extends FreeBoolSimple[Nothing]
    case object TruePred extends ConstantFreeBoolSimple with NotAndPred with NotOrPred
    case object FalsePred extends ConstantFreeBoolSimple with NotAndPred with NotOrPred

    case class Pred[+P](p: P) extends FreeBoolSimple[P] with NotAndPred with NotOrPred
    case class Negate[+P](term: FreeBoolSimple[P]) extends FreeBoolSimple[P] with NotAndPred with NotOrPred
    case class AndPred[+P](terms: CovariantSet[FreeBoolSimple[P] with NotAndPred]) extends FreeBoolSimple[P] with NotOrPred
    case class OrPred[+P](terms: CovariantSet[FreeBoolSimple[P] with NotOrPred]) extends FreeBoolSimple[P] with NotAndPred
  }
  import FreeBoolSimple._

  private implicit def FreeBoolSimpleOrder[P](implicit o: Order[P]) = new Order[FreeBoolSimple[P]] {
    import scalaz.Ordering._

    def order(x: FreeBoolSimple[P], y: FreeBoolSimple[P]): Ordering = (x,y) match {
      case (FalsePred, FalsePred) => EQ
      case (FalsePred, TruePred) => LT
      case (TruePred, FalsePred) => GT
      case (x:ConstantFreeBoolSimple, _) => LT
      case (_, x:ConstantFreeBoolSimple) => LT
      case (Pred(p1), Pred(p2)) => o.order(p1, p2)
      case (Pred(_), _) => LT
      case (_, Pred(_)) => GT
      case (Negate(f1), Negate(f2)) => order(f1, f2)
      case (Negate(_), _) => LT
      case (_, Negate(_)) => GT
      case (AndPred(x), AndPred(y)) => {
        if (x.size > y.size) {
          GT
        } else if (x.size < y.size) {
          LT
        } else {
          val pairs = x.toList.zip(y.toList).iterator
          var result: Ordering = EQ
          while (pairs.hasNext && (result != EQ)) {
            val t: (FreeBoolSimple[P], FreeBoolSimple[P]) = pairs.next
            result = order(t._1, t._2)
          }
          result
        }
      }
      case (AndPred(_), _) => LT
      case (_, AndPred(_)) => GT
      case (OrPred(x), OrPred(y)) => {
        if (x.size > y.size) {
          GT
        } else if (x.size < y.size) {
          LT
        } else {
          val pairs = x.toList.zip(y.toList).iterator
          var result: Ordering = EQ
          while (pairs.hasNext && (result != EQ)) {
            val t: (FreeBoolSimple[P], FreeBoolSimple[P]) = pairs.next
            result = order(t._1, t._2)
          }
          result
        }
      }
    }
  }

  def FreeBoolSimpleAlgebra[P](implicit o: Order[P]) = new ConcreteFreeBoolAlgebra[P, FreeBoolSimple] {
    private implicit val FreeBoolOrder = FreeBoolSimpleOrder[P]

    private implicit val FreeBoolOrderNotAnd: Order[FreeBoolSimple[P] with NotAndPred] = new Order[FreeBoolSimple[P] with NotAndPred] {
      def order(x: FreeBoolSimple[P] with NotAndPred, y: FreeBoolSimple[P] with NotAndPred) = FreeBoolOrder.order(x,y)
    }

    private implicit val FreeBoolOrderNotOr: Order[FreeBoolSimple[P] with NotOrPred] = new Order[FreeBoolSimple[P] with NotOrPred] {
      def order(x: FreeBoolSimple[P] with NotOrPred, y: FreeBoolSimple[P] with NotOrPred) = FreeBoolOrder.order(x,y)
    }

    val bool = new Bool[FreeBoolSimple[P]] {
      def and(a: FreeBoolSimple[P], b: FreeBoolSimple[P]) = (a,b) match {
        case (FalsePred, _) => FalsePred
        case (_, FalsePred) => FalsePred
        case (TruePred, x:AndPred[P]) => x
        case (x: AndPred[P], TruePred) => x
        case (AndPred(terms1), AndPred(terms2)) => AndPred(terms1 ++ terms2)
        case (AndPred(terms), (x: NotAndPred)) => AndPred(terms.insert(x))
        case (x:NotAndPred, AndPred(terms)) => AndPred(terms.insert(x))
        case (x:NotAndPred,y:NotAndPred) => AndPred(CovariantSet(x,y))
      }
      def or(a: FreeBoolSimple[P], b: FreeBoolSimple[P]) = (a,b) match {
        case (TruePred, _) => TruePred
        case (_, TruePred) => TruePred
        case (FalsePred, x:AndPred[P]) => x
        case (x: OrPred[P], FalsePred) => x
        case (OrPred(terms1), OrPred(terms2)) => OrPred(terms1 ++ terms2)
        case (OrPred(terms), (x:NotOrPred)) => OrPred(terms.insert(x))
        case (x:NotOrPred, OrPred(terms)) => OrPred(terms.insert(x))
        case (x:NotOrPred,y:NotOrPred) => OrPred(CovariantSet(x,y))
      }
      def complement(a: FreeBoolSimple[P]) = a match {
        case Negate(x) => x
        case FalsePred => TruePred
        case TruePred => FalsePred
        case x => Negate(x)
      }
      def zero: FreeBoolSimple[P] = FalsePred: FreeBoolSimple[P]
      def one: FreeBoolSimple[P] = TruePred
    }

    def nat[B](f: P => B)(implicit ba: Bool[B]): (FreeBoolSimple[P] => B) = {
      /* For any function f: A => B, B a boolean algebra, there exists a natural homomorphism
       * from the free boolean algebra to the algebra of B.
       */
      def homo(a: FreeBoolSimple[P]): B = a match {
        case Pred(x) => f(x)
        case Negate(x) => ba.complement(homo(x))
        case AndPred(terms) => terms.foldLeft(ba.one)( (x:B,y:FreeBoolSimple[P]) => ba.and(x, homo(y)) )
        case OrPred(terms) => terms.foldLeft(ba.zero)( (x:B,y:FreeBoolSimple[P]) => ba.or(x, homo(y)) )
        case TruePred => ba.one
        case FalsePred => ba.zero
      }
      homo
    }
  }
}
