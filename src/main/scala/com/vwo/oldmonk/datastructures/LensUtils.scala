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

  private implicit class NullToOption[T](val result: T) extends AnyVal {
    def nullToOption: Option[T] = if (result != null) { Some(result) } else { None }
  }

  def singletonCachedLens[A,B<:AnyRef,K<:AnyRef](set: (A,B) => A, get: A => B, cacheKey: A => K): Lens[A,B] = {
    import java.lang.ref.WeakReference

    var keyWeakRef: Option[WeakReference[K]] = None
    var valueWeakRef: Option[WeakReference[B]] = None

    Lens.lensu[A,B](
      (a: A, b: B) => {
        val result = set(a,b)
        keyWeakRef = Some(new WeakReference(cacheKey(result)))
        valueWeakRef = Some(new WeakReference(b))
        result
      },
      (a: A) => {
        val key = cacheKey(a)
        val cachedResult = for {
          oldKey <- keyWeakRef.flatMap(r => r.get().nullToOption)
          oldValue <- if (key eq oldKey) { valueWeakRef.flatMap(r => r.get().nullToOption) } else { None }
        } yield (oldValue)
        cachedResult.fold({
          keyWeakRef = Some(new WeakReference(key))
          val result = get(a)
          valueWeakRef = Some(new WeakReference(result))
          result
        })(ov => ov)
      }
    )
  }
}
