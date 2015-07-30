# Oldmonk

This module provides various utilities, mostly with a mathematical bent, for writing scala programs.

# Data structures
## Covariant set

The `Set[A]` data type in scala is invariant. A `CovariantSet[A]` is, as you might expect, covariant in `A`. It is more or less a small modification of Scalaz's [ISet](https://github.com/scalaz/scalaz/blob/series/7.2.x/core/src/main/scala/scalaz/ISet.scala), modified to be covariant.

As a result, this means that `member` method essentially has type `Any => Boolean`. That's not quite true - the actual type is `member[B >: A]: B => Boolean`, but `B` must have an `Order[B]` instance.

## LensUtils

Unfortunately, using Lenses can involve a lot of duplicated effort relative to using `lazy val` on an object. To eliminate some of this added work, we provide two
replacements for the existing scalaz `Lens`, which avoid this effort.

```scala
def cachedLens[A,B <: AnyRef,K <: AnyRef](set: (A,B) => A, get: A => B, cacheKey: A => K, maxCacheSize: Int = 512, maxCacheDurationSeconds: Int = 10*60)
```

This method will cache the results of calling `get` using a google guava cache. The key to the cache will be computed via the `cacheKey` argument.

```scala
def singletonCachedLens[A,B <: AnyRef,K <: AnyRef](set: (A,B) => A, get: A => B, cacheKey: A => K)
```

This method will cache only the *last* result computed, and the cache will be stored with weak references. Cache equality is computed via
*memory location equality*, i.e. using `x eq y` rather than `x == y`. The rationale for this is that many times, a single lens will be used
in a lot of (mostly unrelated) locations in rapid succession. E.g.:

```scala
val myLens: Lens[Event,P] = LensUtils.singletonCachedLens(...)

def checkEventForP1(event: Event) = if (myLens.get(event) == ...)
def checkEventForP2(event: Event) = if (myLens.get(event) == ...)

...later...

def processEvent(event: Event) = {
  if (checkEventForP1(event)) { p1Handler ! event }
  if (checkEventForP2(event)) { p2Handler ! event }
}
```

# Free objects

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

# Deduplication

## Deduplicator
A couple of useful deduplication objects can be found here. To save memory, there is the `Deduplicator[T]: T => T` which lifts semantic equality to memory equality:

```scala
    val f = Deduplicator[List[String]]()
    f(x) === x
    f(x) eq x
    f(x) eq f(f(x))
```

(There are of course cache-based limitations on this.)

Here, `eq` means that the object on the left has the same memory location as the object on the right. Semantically a deduplicator is the identity function.

## IdempotentEffect

There is also the `IdempotentEffect` which is a way of reducing the number of times an effectful function is called.

```scala
    var cnt: Int = 0
    val f = IdempotentEffect((k:String) => {
      cnt = cnt + 1
    })
    f("foo")
    f("foo")
    cnt === 1
```

`IdempotentEffect` is useful when we want to run a function only once per input - an example use case might be:

```scala
val markEventAsOccurredInDatabase: Event => Unit = IdempotentEffect((e:Event) => {...})
```

Repeated calls to this function for the same event will not cause repeated round trips to the database.

## DelayedIdempotentEffect

There is also the `DelayedIdempotentEffect`. The method `DelayedIdempotentEffect.apply` will return an `Option[DelayedIdempotentEffect.IdempotentMarker]` object. Future calls to the effectful method will only be called *after* the `IdempotentMarker` is called. A concrete example illustrating both how and why:

```scala
val insertIntoDatabase = DelayedIdempotentEffect[X,java.sql.Connection]((x,conn) => insertIntoDatabaseNoCommit(x, conn))
...

val conn = databse.getConnection()
val marker = insertIntoDatabase(key, conn)
...
... // Future calls to insertIntoDatabase(key) will attempt to insert
connection.commit()
marker.foreach(x => x()) //Now, *after* the insert is committed, future attempts will be prevented
```

Deduplication is handled based on the *first* argument, the second argument is ignored. So repeated calls to `insertIntoDatabase` with the *same* value of `x` but different values of `conn` will be deduplicated.

There is also a `DelayedIdempotentEffect.simple[X](x => f(x))` method which ignores the other argument.
