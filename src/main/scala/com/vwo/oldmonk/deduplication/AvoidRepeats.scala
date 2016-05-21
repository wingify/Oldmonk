package com.vwo.oldmonk.deduplication

import com.vwo.oldmonk.datastructures.{RotatingBloomFilter, SemiConcurrentBloomFilter}

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

    private val bf = new RotatingBloomFilter(insertionsBeforeRotate, falsePositiveProbability)

    def apply(key: A): Unit = {
      if (!bf.mightContain(key)) {
        f(key)
        bf.add(key)
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

    def concurrentProbabilistic[A, B](f: (A,B) => Unit, filterSize: Int = 1024*1024, falsePositiveProbability: Double = 1e-12, insertionsBeforeCompact: Int = 1024)(implicit funnel: Funnel[A]): DelayedIdempotentEffect[A,B] = new DelayedIdempotentEffect[A,B] {
      import java.util.concurrent.atomic._

      private def newFilter = new SemiConcurrentBloomFilter[A](filterSize*2, falsePositiveProbability, insertionsBeforeCompact)(funnel)

      private val filter1 = new AtomicReference[SemiConcurrentBloomFilter[A]](newFilter)
      private val filter2 = new AtomicReference[SemiConcurrentBloomFilter[A]](newFilter)
      private val insertions = new AtomicLong(0)

      private def rotate = {
        val f1 = filter1.get()
        filter2.set(f1)
        filter1.set(newFilter)
      }

      private val self = this
      private var requiresRotate = new AtomicBoolean(false)

      private case class ConcreteDelayedIdempotent(key: A) extends IdempotentMarker[A] {
        def apply() = {
          val f1 = filter1.get()
          f1.add(key)
          val inserted = insertions.incrementAndGet()
          if (inserted % filterSize == 0) {
            requiresRotate.set(true)
            self.synchronized {
              val needsRotate = requiresRotate.get()
              if (needsRotate) {
                rotate
                insertions.set(0)
                requiresRotate.set(false)
              }
            }
          }
        }
      }

      private def mightContain(key: A) = (!filter1.get().mightContain(key)) && (!filter2.get().mightContain(key))

      def apply(key: A, otherArgs: B) = {
        val runAction = mightContain(key)
        if (runAction) {
          f(key, otherArgs)
          Some(ConcreteDelayedIdempotent(key)) : Option[IdempotentMarker[A]]
        } else {
          None
        }
      }
    }
  }
}
