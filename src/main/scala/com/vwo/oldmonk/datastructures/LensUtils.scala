package com.vwo.oldmonk.datastructures

import com.google.common.cache.{Cache, CacheBuilder}

import scalaz._
import Scalaz._

object LensUtils {
  def cachedLens[A,B <: AnyRef,K <: AnyRef](set: (A,B) => A, get: A => B, cacheKey: A => K, maxCacheSize: Int = 512, maxCacheDurationSeconds: Int = 10*60): Lens[A,B] = {
    val getCache: Cache[K,B] = CacheBuilder.newBuilder().maximumSize(maxCacheSize).expireAfterWrite(maxCacheDurationSeconds, java.util.concurrent.TimeUnit.SECONDS).build()

    Lens.lensu[A,B](
      set,
      (a: A) => {
        val k = cacheKey(a)
        var result = getCache.getIfPresent(k)
        if (result == null) {
          result = get(a)
          getCache.put(k, result)
        }
        result
      }
    )
  }
}
