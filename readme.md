# Oldmonk

This module provides various utilities, mostly with a mathematical bent, for writing scala programs.

## Data structures
### Covariant set

The `Set[A]` data type in scala is invariant. A `CovariantSet[A]` is, as you might expect, covariant in `A`. It is more or less a small modification of Scalaz's [ISet](https://github.com/scalaz/scalaz/blob/series/7.2.x/core/src/main/scala/scalaz/ISet.scala), modified to be covariant.

As a result, this means that `member` method essentially has type `Any => Boolean`. That's not quite true - the actual type is `member[B >: A]: B => Boolean`, but `B` must have an `Order[B]` instance.

## Free objects

A free object is a functor together with a natural transformation that preserves it's structure. It can be thought of as the most expansive possible version of that structure, only obeying the minimum laws necessary.

Somewhat concretely:

```scala
trait FreeThing[F[_]] extends Functor[F] {
  implicit def structure[A]: Structure[F[A]]
  def point[A](a: =>A): F[A]

  def nat[A,B](f: A => B)(implicit s: Structure[B]): F[A] => B
}
```

Here, "structure" can be a `Monoid`, a `Group`, a `Bool`, etc - basically any useful algebraic structure. The key fact about the `nat` operation is that `nat(f)` must be a homomorphism, i.e. it must preserve the operations of that structure. For example, for a `Monoid`:

```scala
nat(f)(x |+| y) = nat(f)(x) |+| nat(f)(y)
```

It must also commute with `point`:

```scala
nat(f)(point[A](x)) = point[B](f(x))
```

Another way to think of it is that it's a data structure with no interpretation.
