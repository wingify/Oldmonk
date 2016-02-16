package com.vwo.oldmonk.datastructures

import com.google.common.hash._
import com.google.common.base.Predicate

class RotatingBloomFilter[A](insertionsBeforeRotate: Int = 1024*1024, falsePositiveProbability: Double = 1e-12)(implicit funnel: Funnel[A]) extends Predicate[A] {
  private var mainFilter = BloomFilter.create[A](funnel, insertionsBeforeRotate, falsePositiveProbability)
  private var oldFilter = BloomFilter.create[A](funnel, insertionsBeforeRotate, falsePositiveProbability)
  private var numInsertions: Int = 0

  def apply(a: A) = mightContain(a)

  def mightContain(a: A) = oldFilter.mightContain(a) || mainFilter.mightContain(a)

  def add(a: A) = {
    mainFilter.put(a)
    numInsertions += 1
    if (numInsertions > insertionsBeforeRotate) {
      oldFilter = mainFilter
      mainFilter = BloomFilter.create[A](funnel, insertionsBeforeRotate, falsePositiveProbability)
      numInsertions = 0
    }
  }
}
