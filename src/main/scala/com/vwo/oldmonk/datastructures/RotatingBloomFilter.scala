package com.vwo.oldmonk.datastructures

import com.google.common.hash._
import com.google.common.base.Predicate

class RotatingBloomFilter[A](insertionsBeforeRotate: Int = 1024*1024, falsePositiveProbability: Double = 1e-12)(implicit funnel: Funnel[A]) extends Predicate[A] {
  /*
   * A pair of bloom filters, once one fills up it's rotated into the second position
   * and a new one is created. So basically it's a bloom filter but applied to a rolling
   * window.
   */

  protected var mainFilter = BloomFilter.create[A](funnel, insertionsBeforeRotate, falsePositiveProbability)
  protected var oldFilter = BloomFilter.create[A](funnel, insertionsBeforeRotate, falsePositiveProbability)
  protected var numInsertions: Int = 0

  def apply(a: A) = mightContain(a)

  def mightContain(a: A) = oldFilter.mightContain(a) || mainFilter.mightContain(a)

  def add(a: A) = {
    mainFilter.put(a)
    numInsertions += 1
    if (numInsertions > insertionsBeforeRotate) {
      rotate
    }
  }

  def rotate = {
    oldFilter = mainFilter
    mainFilter = BloomFilter.create[A](funnel, insertionsBeforeRotate, falsePositiveProbability)
    numInsertions = 0
  }

  def clear = {
    rotate
    rotate
  }
}

class TimedRotatingBloomFilter[A](insertionsBeforeRotate: Int = 1024*1024, falsePositiveProbability: Double = 1e-12, rotateTimeMillis: Long = 1000*60*60*24)(implicit funnel: Funnel[A]) extends RotatingBloomFilter(insertionsBeforeRotate, falsePositiveProbability)(funnel) {
  /*
   * Similar functionality as for rotating bloom filter, but also rotations happen at fixed intervals.
   */
  private var lastRotateTime = System.currentTimeMillis

  override def mightContain(a: A) = {
    rotateByTime
    super.mightContain(a)
  }

  private def rotateByTime = {
    if (System.currentTimeMillis > lastRotateTime + rotateTimeMillis) {
      rotate
      rotate
    } else if (System.currentTimeMillis > lastRotateTime + rotateTimeMillis) {
      rotate
    }
  }

  override def add(a: A) = {
    mainFilter.put(a)
    numInsertions += 1
    if (numInsertions > insertionsBeforeRotate) {
      rotate
    }
    rotateByTime
  }

  override def rotate = {
    super.rotate
    lastRotateTime = System.currentTimeMillis
  }
}
