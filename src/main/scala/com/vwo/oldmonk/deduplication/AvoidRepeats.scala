package com.vwo.oldmonk.deduplication

import com.google.common.cache._
import java.util.concurrent.TimeUnit
import com.google.common.hash._

trait ProvidesAvoidRepeats {

  trait IdempotentEffect[A] extends (A => Unit) {
    // This function represents a side-effecting method which should be called only once.
    def apply(key: A): Unit
  }

  object IdempotentEffect {
    def apply[A <: Object](f: A => Unit, timeout: Int = 24*60*60, maxSize: Int = 1024): IdempotentEffect[A] = new IdempotentEffect[A] {
      private val cache: Cache[A,A] = CacheBuilder.newBuilder().maximumSize(maxSize).expireAfterWrite(timeout, TimeUnit.SECONDS).build()
      def apply(key: A): Unit = {
        if (cache.getIfPresent(key) == null) {
          f(key)
          cache.put(key, key)
        }
        ()
      }
    }
  }

  class BloomFilterIdempotentEffect[A](f: A => Unit, insertionsBeforeRotate: Int = 1024*1024, falsePositiveProbability: Double = 1e-12)(implicit funnel: Funnel[A]) extends IdempotentEffect[A] {
    /* Note that this class is single-threaded.
     * However, it provides far better memory usage than IdempotentEffect.
     */

    private var mainFilter = BloomFilter.create[A](funnel, insertionsBeforeRotate, falsePositiveProbability)
    private var oldFilter = BloomFilter.create[A](funnel, insertionsBeforeRotate, falsePositiveProbability)
    private var numInsertions: Int = 0

    private def add(key: A) = {
      mainFilter.put(key)
      numInsertions += 1
      if (numInsertions > insertionsBeforeRotate) {
        oldFilter = mainFilter
        mainFilter = BloomFilter.create[A](funnel, insertionsBeforeRotate, falsePositiveProbability)
        numInsertions = 0
      }
    }

    def apply(key: A): Unit = {
      if (!mainFilter.mightContain(key) && !oldFilter.mightContain(key)) {
        f(key)
        add(key)
      }
      ()
    }
  }

  trait DelayedIdempotentEffect[A,B] {
    trait IdempotentMarker[A] {
      val key: A
      def apply(): Unit //After apply is called, future calls to super.apply() should have no side effect.
    }
    // This function represents a side-effecting method which should be called only once.
    def apply(key: A, otherArgs: B): Option[IdempotentMarker[A]]
  }

  trait DelayedIdempotentEffectNoOtherArg[A] extends DelayedIdempotentEffect[A,Unit] {
    def apply(key: A): Option[IdempotentMarker[A]] = apply(key, ())
  }

  object DelayedIdempotentEffect {

    def simple[A <: Object](f: (A) => Unit, timeout: Int = 24*60*60, maxSize: Int = 1024): DelayedIdempotentEffectNoOtherArg[A] = new DelayedIdempotentEffectNoOtherArg[A] {
      private val cache: Cache[A,A] = CacheBuilder.newBuilder().maximumSize(maxSize).expireAfterWrite(timeout, TimeUnit.SECONDS).build()

      private case class ConcreteDelayedIdempotent(key: A) extends IdempotentMarker[A] { self =>
        def apply() = cache.put(key, key)
      }

      def apply(key: A, otherArgs: Unit): Option[IdempotentMarker[A]] = {
        val fromCache = cache.getIfPresent(key)
        if (fromCache == null) {
          f(key)
          Some(ConcreteDelayedIdempotent(key)) : Option[IdempotentMarker[A]]
        } else {
          None
        }
      }
    }

    def apply[A <: Object, B](f: (A,B) => Unit, timeout: Int = 24*60*60, maxSize: Int = 1024): DelayedIdempotentEffect[A,B] = new DelayedIdempotentEffect[A,B] {
      private val cache: Cache[A,A] = CacheBuilder.newBuilder().maximumSize(maxSize).expireAfterWrite(timeout, TimeUnit.SECONDS).build()

      private case class ConcreteDelayedIdempotent(key: A) extends IdempotentMarker[A] {
        def apply() = cache.put(key, key)
      }

      def apply(key: A, otherArgs: B) = {
        val fromCache = cache.getIfPresent(key)
        if (fromCache == null) {
          f(key, otherArgs)
          Some(ConcreteDelayedIdempotent(key)) : Option[IdempotentMarker[A]]
        } else {
          None
        }
      }
    }
  }
}
