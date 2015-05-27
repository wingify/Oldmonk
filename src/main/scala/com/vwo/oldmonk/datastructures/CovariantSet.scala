package com.vwo.oldmonk.datastructures

import scala.math.Ordering
import scalaz._
import Scalaz._

import annotation.tailrec

object CovariantSet extends CovariantSetInstances with CovariantSetFunctions {
   private[datastructures] abstract case class Tip[A] private() extends CovariantSet[A] {
    val size = 0
  }
  private[datastructures] object Tip extends Tip[Nothing] {
    def apply[A](): CovariantSet[A] = this.asInstanceOf[CovariantSet[A]]
  }

  private[datastructures] final case class Bin[A](a: A, l: CovariantSet[A], r: CovariantSet[A]) extends CovariantSet[A] {
    val size = l.size + r.size + 1
  }

  def apply[A](x: A*)(implicit order: Order[A]) = x.size match {
    case 0 => empty
    case 1 => singleton(x(0))
    case _ => fromList(x.toList)
  }
}

sealed abstract class CovariantSet[+A] {
  import scalaz.Ordering._
  import CovariantSet._

  val size: Int

  // -- * Query
  final def isEmpty =
    this match {
      case Tip() => true
      case Bin(_, _, _) => false
    }

  @tailrec
  final def member[B >: A](x: B)(implicit o: Order[B]): Boolean =
    this match {
      case Tip() => false
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT => l.member(x)
          case GT => r.member(x)
          case EQ => true
        }
    }

  /** Alias for member */
  final def contains[B >: A](x: B)(implicit o: Order[B]) =
    member(x)

  final def notMember[B >: A](x: B)(implicit o: Order[B]) =
    !member(x)

  final def isSubsetOf[B >: A](other: CovariantSet[B])(implicit o: Order[B]) =
    (this.size <= other.size) && this.isSubsetOfX(other)

  private def isSubsetOfX[B >: A](other: CovariantSet[B])(implicit o: Order[B]): Boolean =
    (this, other) match {
      case (Tip(), _) =>
        true
      case (_, Tip()) =>
        false
      case (Bin(x, l, r), t) =>
        val (lt,found,gt) = t.splitMember(x)
        found && l.isSubsetOfX(lt) && r.isSubsetOfX(gt)
    }

  final def isProperSubsetOf[B >: A](other: CovariantSet[B])(implicit o: Order[B]) =
    (this.size < other.size) && this.isSubsetOf(other)

  // -- * Construction
  final def insert[B >: A](x: B)(implicit o: Order[B]): CovariantSet[B] =
    this match {
      case Tip() => singleton(x)
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT => balanceL(y, l.insert(x), r)
          case GT => balanceR(y, l, r.insert(x))
          case EQ => Bin(x, l, r)
        }
    }

  final def delete[B >: A](x: B)(implicit o: Order[B]): CovariantSet[A] =
    this match {
      case Tip() =>
        Tip()
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT =>
            balanceR(y, l.delete(x), r)
          case GT =>
            balanceL(y, l, r.delete(x))
          case EQ =>
            glue(l, r)
        }
    }

  final def ++[B >: A](other: CovariantSet[B])(implicit o: Order[B]): CovariantSet[B] = union(other)

  // -- * Combine
  final def union[B >: A](other: CovariantSet[B])(implicit o: Order[B]): CovariantSet[B] = {
    def hedgeUnion(blo: Option[B], bhi: Option[B], t1: CovariantSet[B], t2: CovariantSet[B])(implicit o: Order[B]): CovariantSet[B] =
      (t1, t2) match {
        case (t1, Tip()) =>
          t1
        case (Tip(), Bin(x, l, r)) =>
          join(x, l.filterGt(blo), r.filterLt(bhi))
        case (_, Bin(x, Tip(), Tip())) =>
          t1.insertR(x)
        case (Bin(x, l, r), _) =>
          val bmi = some(x)
          join(x, hedgeUnion(blo, bmi, l, t2.trim(blo, bmi)), hedgeUnion(bmi, bhi, r, t2.trim(bmi, bhi)))
      }

    (this, other) match {
      case (Tip(), t2) =>
        t2
      case (t1, Tip()) =>
        t1
      case (t1, t2) =>
        hedgeUnion(none, none, t1, t2)
    }
  }

  private def insertR[B >: A](x: B)(implicit o: Order[B]): CovariantSet[B] =
    this match {
      case Tip() =>
        singleton(x)
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT =>
            balanceL(y, l.insertR(x), r)
          case GT =>
            balanceR(y, l, r.insertR(x))
          case EQ =>
            this
        }
    }

  // -- * Filter
  final def filter(p: A => Boolean): CovariantSet[A] =
    this match {
      case Tip() => this
      case Bin(x, l, r) =>
        if (p(x)) join(x, l.filter(p), r.filter(p)) else l.filter(p) merge r.filter(p)
    }

  final def partition(p: A => Boolean): (CovariantSet[A], CovariantSet[A]) =
    this match {
      case Tip() =>
        (this, this)
      case Bin(x, l, r) =>
        val (l1, l2) = l.partition(p)
        val (r1, r2) = r.partition(p)
        if (p(x)) (join(x, l1, r1), l2 merge r2) else (l1 merge r1, join(x, l2, r2))
    }

  final def splitMember[B >: A](x: B)(implicit o: Order[B]): (CovariantSet[A], Boolean, CovariantSet[A]) =
    this match {
      case Tip() =>
        (this, false, this)
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT =>
            val (lt, found, gt) = l.splitMember(x)
            (lt, found, join(y, gt, r))
          case GT =>
            val (lt, found, gt) = r.splitMember(x)
            (join(y, l, lt), found, gt)
          case EQ =>
            (l, true, r)
        }
    }

  /**
    * For the `Functor` composition law to hold it is important that the `Order[B]` is substitutive for the `Order[A]` –
    * that is, that the `Order[B]` should be __no stronger__, it should not distinguish two `B` instances that would
    * be considered as equal `A` instances.
    *
    * '''Note:''' this is not able to implement `Functor` due to the `Order` constraint on the destination type,
    * however it still is a functor in the mathematical sense.
    *
    * Documentation as copied from the Haskell source:
    *  {{{
    -- | /O(n*log n)/.
    -- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
    --
    -- It's worth noting that the size of the result may be smaller if,
    -- for some @(x,y)@, @x \/= y && f x == f y@
    }}}
    */
  def map[B: Order](f: A => B) =
    fromList(toList.map(f))

  // -- * Folds
  final def foldRight[B](z: B)(f: (A, B) => B): B =
    this match {
      case Tip() => z
      case Bin(x, l ,r) => l.foldRight(f(x, r.foldRight(z)(f)))(f)
    }

  final def foldr[B](z: B)(f: (A, B) => B): B =
    foldRight(z)(f)

  final def foldLeft[B](z: B)(f: (B, A) => B): B =
    this match {
      case Tip() => z
      case Bin(x, l, r) =>
        r.foldLeft(f(l.foldLeft(z)(f), x))(f)
    }

  final def foldl[B](z: B)(f: (B, A) => B): B =
    foldLeft(z)(f)

  // -- * Min\/Max
  @tailrec
  final def findMin: Option[A] =
    this match {
      case Tip() => none
      case Bin(x, Tip(), _) => some(x)
      case Bin(_, l, _) => l.findMin
    }

  @tailrec
  final def findMax: Option[A] =
    this match {
      case Tip() => none
      case Bin(x, _, Tip()) => some(x)
      case Bin(_, _, r) => r.findMax
    }

  final def deleteMin: CovariantSet[A] =
    this match {
      case Bin(_, Tip(), r) => r
      case Bin(x, l, r) => balanceR(x, l.deleteMin, r)
      case Tip() => Tip()
    }

  final def deleteMax: CovariantSet[A] =
    this match {
      case Bin(_, l, Tip()) => l
      case Bin(x, l, r) => balanceL(x, l, r.deleteMax)
      case Tip() => Tip()
    }

  // TODO: Can we make this total? or should this remain unsafe, preferring minView instead?
  final def deleteFindMin: (A, CovariantSet[A]) =
    this match {
      case Bin(x, Tip(), r) => (x, r)
      case Bin(x, l, r) =>
        val (xm, l2) = l.deleteFindMin
        (xm, balanceR(x, l2, r))
      case Tip() => sys.error("deleteFindMin on empty CovariantSet")
    }

  // TODO: Can we make this total? or should this remain unsafe, preferring maxView instead?
  final def deleteFindMax: (A, CovariantSet[A]) =
    this match {
      case Bin(x, l, Tip()) => (x, l)
      case Bin(x, l, r) =>
        val (xm, r2) = r.deleteFindMax
        (xm, balanceL(x, l, r2))
      case Tip() => sys.error("deleteFindMax on empty CovariantSet")
    }

  final def minView: Option[(A, CovariantSet[A])] =
    this match {
      case Tip() => none
      case x => some(x.deleteFindMin)
    }

  final def maxView: Option[(A, CovariantSet[A])] =
    this match {
      case Tip() => none
      case x => some(x.deleteFindMax)
    }

  // -- ** List
  final def elems =
    toAscList

  final def toList =
    toAscList

  // -- ** Ordered list
  final def toAscList =
    foldRight(List.empty[A])(_ :: _)

  final def toDescList =
    foldLeft(List.empty[A])((a, b) => b :: a)

  private def glue[A](l: CovariantSet[A], r: CovariantSet[A]): CovariantSet[A] =
    (l, r) match {
      case (Tip(), r) => r
      case (l, Tip()) => l
      case (_, _) =>
        if (l.size > r.size) {
          val (m, l2) = l.deleteFindMax
          balanceR(m, l2, r)
        } else {
          val (m, r2) = r.deleteFindMin
          balanceL(m, l, r2)
        }
    }

  private def join[A](x: A, l: CovariantSet[A], r: CovariantSet[A]): CovariantSet[A] =
    (l, r) match {
      case (Tip(), r) => r.insertMin(x)
      case (l, Tip()) => l.insertMax(x)
      case (Bin(y, ly, ry), Bin(z, lz, rz)) =>
        if (delta*l.size < r.size) balanceL(z, join(x, l, lz), rz)
        else if (delta*r.size < l.size) balanceR(y, ly, join(x, ry, r))
        else Bin(x, l, r)
    }

  private def insertMax[B >: A](x: B): CovariantSet[B] =
    this match {
      case Tip() =>
        singleton(x)
      case Bin(y, l, r) =>
        balanceR(y, l, r.insertMax(x))
    }

  private def insertMin[B >: A](x: B): CovariantSet[B] =
    this match {
      case Tip() =>
        singleton(x)
      case Bin(y, l, r) =>
        balanceL(y, l.insertMin(x), r)
    }

  protected def merge[B >: A](other: CovariantSet[B]): CovariantSet[B] =
    (this, other) match {
      case (Tip(), r) => r
      case (l, Tip()) => l
      case (l@Bin(x, lx: CovariantSet[B], rx: CovariantSet[B]), r@Bin(y, ly: CovariantSet[B], ry: CovariantSet[B])) =>
        if (delta*l.size < r.size) balanceL(y, l merge ly, ry)
        else if (delta*r.size < l.size) balanceR(x, lx, rx merge r)
        else glue(l, r)
    }

  final def trim[B >: A](a: Option[B], b: Option[B])(implicit o: Order[B]): CovariantSet[A] =
    (a, b) match {
      case (None, None) =>
        this
      case (Some(lx), None) =>
        def greater(lo: B, t: CovariantSet[A]): CovariantSet[A] =
          t match {
            case Bin(x, _, r) => if (o.lessThanOrEqual(x, lo)) greater(lo, r) else t
            case _ => t
          }
        greater(lx, this)
      case (None, Some(hx)) =>
        def lesser(hi: B, t: CovariantSet[A]): CovariantSet[A] =
          t match {
            case Bin(x, l, _) => if (o.greaterThanOrEqual(x, hi)) lesser(hi, l) else t
            case _ => t
          }
        lesser(hx, this)
      case (Some(lx), Some(rx)) =>
        def middle(lo: B, hi: B, t: CovariantSet[A]): CovariantSet[A] =
          t match {
            case Bin(x, l, r) =>
              if (o.lessThanOrEqual(x, lo)) middle(lo, hi, r)
              else if (o.greaterThanOrEqual(x, hi)) middle(lo, hi, l)
              else t
            case _ => t
          }
        middle(lx, rx, this)
    }

  final def filterGt[B >: A](a: Option[B])(implicit o: Order[B]): CovariantSet[A] =
    cata(a)(s => this match {
      case Tip() => CovariantSet.empty
      case Bin(x, l, r) =>
        o.order(s, x) match {
          case LT => join(x, l.filterGt(a), r)
          case EQ => r
          case GT => r.filterGt(a)
        }
    }, this)

  final def filterLt[B >: A](a: Option[B])(implicit o: Order[B]): CovariantSet[A] =
    cata(a)(s => this match {
      case Tip() => CovariantSet.empty
      case Bin(x, l, r) =>
        o.order(x, s) match {
          case LT => join(x, l, r.filterLt(a))
          case EQ => l
          case GT => l.filterLt(a)
        }
    }, this)

  override final def equals(other: Any): Boolean =
    other match {
      case that: CovariantSet[A] =>
        CovariantSet.setEqual[A](Equal.equalA).equal(this, that)
      case _ =>
        false
    }

  override final def hashCode: Int =
    toAscList.hashCode
}

sealed trait CovariantSetFunctions {
  import CovariantSet._

  final def empty[A]: CovariantSet[A] =
    Tip()

  final def singleton[A](x: A): CovariantSet[A] =
    Bin(x, Tip(), Tip())

  final def fromList[A](xs: List[A])(implicit o: Order[A]): CovariantSet[A] =
    xs.foldLeft(empty[A])((a, b) => a insert b)

  final def fromFoldable[F[_], A](xs: F[A])(implicit F: Foldable[F], o: Order[A]): CovariantSet[A] =
    F.foldLeft(xs, empty[A])((a, b) => a insert b)

  final def unions[A](xs: List[CovariantSet[A]])(implicit o: Order[A]): CovariantSet[A] =
    xs.foldLeft(CovariantSet.empty[A])(_ union _)

  private[datastructures] final val delta = 3
  private[datastructures] final val ratio = 2

  private[datastructures] def balanceL[A](x: A, l: CovariantSet[A], r: CovariantSet[A]): CovariantSet[A] =
    r match {
      case Tip() =>
        l match {
          case Tip() =>
            singleton(x)
          case Bin(_, Tip(), Tip()) =>
            Bin(x, l, Tip())
          case Bin(lx, Tip(), Bin(lrx, _, _)) =>
            Bin(lrx, singleton(lx), singleton(x))
          case Bin(lx, ll@Bin(_, _, _), Tip()) =>
            Bin(lx, ll, singleton(x))
          case Bin(lx, ll@Bin(_, _, _), lr@Bin(lrx, lrl, lrr)) =>
            if (lr.size < ratio*ll.size) Bin(lx, ll, Bin(x, lr, Tip()))
            else Bin(lrx, Bin(lx, ll, lrl), Bin(x, lrr, Tip()))
        }
      case Bin(_, _, _) =>
        l match {
          case Tip() =>
            Bin(x, Tip(), r)
          case Bin(lx, ll, lr) =>
            if (l.size > delta*r.size) {
              (ll, lr) match {
                case (Bin(_, _, _), Bin(lrx, lrl, lrr)) =>
                  if (lr.size < ratio*ll.size) Bin(lx, ll, Bin(x, lr, r))
                  else Bin(lrx, Bin(lx, ll, lrl), Bin(x, lrr, r))
                case _ => sys.error("Failure in CovariantSet.balanceL")
              }
            } else Bin(x, l, r)
        }
    }

  private[datastructures] def balanceR[A](x: A, l: CovariantSet[A], r: CovariantSet[A]): CovariantSet[A] =
    l match {
      case Tip() =>
        r match {
          case Tip() =>
            singleton(x)
          case Bin(_, Tip(), Tip()) =>
            Bin(x, Tip(), r)
          case Bin(rx, Tip(), rr@Bin(_, _, _)) =>
            Bin(rx, singleton(x), rr)
          case Bin(rx, Bin(rlx, _, _), Tip()) =>
            Bin(rlx, singleton(x), singleton(rx))
          case Bin(rx, rl@Bin(rlx, rll, rlr), rr@Bin(_, _, _)) =>
            if (rl.size < ratio*rr.size) Bin(rx, Bin(x, Tip(), rl), rr)
            else Bin(rlx, Bin(x, Tip(), rll), Bin(rx, rlr, rr))
        }
      case Bin(_, _, _) =>
        r match {
          case Tip() =>
            Bin(x, l, Tip())
          case Bin(rx, rl, rr) =>
            if (r.size > delta*l.size) {
              (rl, rr) match {
                case (Bin(rlx, rll, rlr), Bin(_, _, _)) =>
                  if (rl.size < ratio*rr.size) Bin(rx, Bin(x, l, rl), rr)
                  else Bin(rlx, Bin(x, l, rll), Bin(rx, rlr, rr))
                case _ => sys.error("Failure in CovariantSet.balanceR")
              }
            } else Bin(x, l, r)
        }
    }
}

sealed abstract class CovariantSetInstances {
  import CovariantSet._

  implicit def setEqual[A: Equal]: Equal[CovariantSet[A]] = new CovariantSetEqual[A] {
    def A = implicitly
  }

  implicit def setOrder[A: Order]: Order[CovariantSet[A]] = new Order[CovariantSet[A]] with CovariantSetEqual[A] {
    import std.list._
    def A = implicitly

    def order(x: CovariantSet[A], y: CovariantSet[A]) =
      Order[List[A]].order(x.toAscList, y.toAscList)
  }

  implicit def setShow[A: Show]: Show[CovariantSet[A]] = new Show[CovariantSet[A]] {
    override def shows(f: CovariantSet[A]) =
      f.toAscList.mkString("CovariantSet(", ",", ")")
  }

  implicit def setMonoid[A: Order]: Monoid[CovariantSet[A]] = new Monoid[CovariantSet[A]] {
    def zero: CovariantSet[A] =
      empty[A]

    def append(a: CovariantSet[A], b: => CovariantSet[A]): CovariantSet[A] =
      a union b
  }

  implicit val setFoldable: Foldable[CovariantSet] = new Foldable[CovariantSet] {
    def foldMap[A, B](fa: CovariantSet[A])(f: A => B)(implicit F: Monoid[B]): B =
      fa match {
        case Tip() =>
          F.zero
        case Bin(x, l, r) =>
          F.append(F.append(foldMap(l)(f), f(x)), foldMap(r)(f))
      }

    def foldRight[A, B](fa: CovariantSet[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)((a, b) => f(a, b))

    override def foldLeft[A, B](fa: CovariantSet[A], z: B)(f: (B, A) => B) =
      fa.foldLeft(z)(f)

    override def length[A](fa: CovariantSet[A]) =
      fa.size

    override def maximum[A: Order](fa: CovariantSet[A]) =
      fa.findMax

    override def minimum[A: Order](fa: CovariantSet[A]) =
      fa.findMin

    override def empty[A](fa: CovariantSet[A]) =
      fa.isEmpty

    override def any[A](fa: CovariantSet[A])(f: A => Boolean) =
      fa match {
        case Tip() => false
        case Bin(x, l, r) =>
          any(l)(f) || f(x) || any(r)(f)
      }

    override def all[A](fa: CovariantSet[A])(f: A => Boolean) =
      fa match {
        case Tip() => true
        case Bin(x, l, r) =>
          all(l)(f) && f(x) && all(r)(f)
      }
  }
}

private sealed trait CovariantSetEqual[A] extends Equal[CovariantSet[A]] {
  import std.list._
  implicit def A: Equal[A]

  override final def equal(a1: CovariantSet[A], a2: CovariantSet[A]) =
    (a1.size == a2.size) && Equal[List[A]].equal(a1.toAscList, a2.toAscList)
}
