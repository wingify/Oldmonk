package com.vwo.oldmonk.deduplication

import com.google.common.cache._
import java.util.concurrent.TimeUnit

trait ProvidesDeduplication {
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
