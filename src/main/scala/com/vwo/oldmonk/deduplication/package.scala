package com.vwo.oldmonk

import com.google.common.cache._
import java.util.concurrent.TimeUnit

package object deduplication {
  trait IdempotentEffect[A] {
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

  trait Deduplicator[T] extends (T => T) {
    // This should be the identity.
    def apply(t: T): T
  }

  object Deduplicator {
    def apply[T <: Object](maxSize: Long = 1024, ttl: Int = 60*24*60): Deduplicator[T] = new Deduplicator[T] {
      private val cache: Cache[T,T] = CacheBuilder.newBuilder().maximumSize(maxSize).expireAfterWrite(ttl, TimeUnit.SECONDS).build()

      def apply(t: T): T = {
        val result = cache.getIfPresent(t)
        if (result != null) {
          result
        } else {
          cache.put(t, t)
          t
        }
      }
    }
  }

}
