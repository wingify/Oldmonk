package com.vwo.oldmonk.datastructures

import com.google.common.hash._

trait CountMinSketchLike[K] {
  val relativeError: Double
  val errorProbability: Double
  val maxL1Result: Long
  def size: Int
  def get(item: K): Long
  def update(item: K, increment: Long): Unit
}

private class BasicCountMinSketchImpl[K](val relativeError: Double, val errorProbability: Double, val maxL1Result: Long, hashFunctionConstructor: Int => HashFunction = x => Hashing.murmur3_128(x))(implicit funnel: Funnel[K]) {
  //SHOULD NEVER BE USED OUTSIDE
  import java.lang.Math
  val depth = Math.ceil(-1*Math.log(errorProbability)).toInt
  val width = Math.ceil(Math.E * maxL1Result / relativeError).toInt
  require(depth > 0)
  require(width > 0)

  protected val counts: Array[Long] = new Array[Long](width*depth)
  def size = counts.size
  protected val hfs: Array[HashFunction] = (0 to (depth-1)).map(hashFunctionConstructor).toArray

  protected def pos(i: Int, j: Int): Int = {
    require(0 <= i)
    require(i < width)
    val jpos = if (j > 0) { j } else { -j }
    i*depth + (jpos % width)
  }

  def get(item: K): Long = {
    val estimates = new Array[Long](depth)
    var i=0
    while (i<depth) {
      val h = hfs(i).newHasher()
      funnel.funnel(item, h)
      val position = pos(i, h.hash().asInt())
      estimates(i) = counts(position)
      i += 1
    }
    estimates.min
  }
}

class CountMinSketch[K](relativeError: Double, errorProbability: Double, maxL1Result: Long, hashFunctionConstructor: Int => HashFunction = x => Hashing.murmur3_128(x))(implicit funnel: Funnel[K]) extends BasicCountMinSketchImpl[K](relativeError, errorProbability, maxL1Result, hashFunctionConstructor)(funnel) with CountMinSketchLike[K]{

  def update(item: K, increment: Long): Unit = {
    require(increment >= 0)
    if (increment == 0) {
      return ()
    }
    var i=0
    while (i < depth) {
      val h = hfs(i).newHasher()
      funnel.funnel(item, h)
      val position = pos(i, h.hash().asInt())
      counts(position) += increment
      i += 1
    }
  }
}

class ConservativeCountMinSketch[K](relativeError: Double, errorProbability: Double, maxL1Result: Long, hashFunctionConstructor: Int => HashFunction = x => Hashing.murmur3_128(x))(implicit funnel: Funnel[K]) extends BasicCountMinSketchImpl[K](relativeError, errorProbability, maxL1Result, hashFunctionConstructor)(funnel) with CountMinSketchLike[K]{

  def update(item: K, increment: Long): Unit = {
    require(increment >= 0)
    if (increment == 0) {
      return ()
    }
    val oldCount = get(item)
    var i=0
    while (i < depth) {
      val h = hfs(i).newHasher()
      funnel.funnel(item, h)
      val position = pos(i, h.hash().asInt())
      counts(position) = Math.max(counts(position), oldCount + increment)
      i += 1
    }
  }
}
