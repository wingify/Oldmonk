package com.vwo.oldmonk.datastructures

import com.google.common.hash._

class InvertedProbabilisticMultiMap[K,V](expectedInsertions: Int = 1024*1024, falsePositiveProbability: Double = 1e-12)(implicit keyFunnel: Funnel[K]) {
  /*
   * Use cases: You'll have a LOT of keys, but very few values. If you have K keys and V values, then the
   * complexity of this class's get and set operations are O(V * log(K))
   * There is also a finite probability of returning the wrong result.
   *
   * Due to the use of bloom filters, it is best to avoid inputting "null-like" objects.
   * For example, an empty string, or a sequence of integers which are all zero.
   * For reasons I can't figure out, they cause this class to return weirdo results.
   */

  private def newBloomFilter: BloomFilter[K] = BloomFilter.create[K](keyFunnel, expectedInsertions, falsePositiveProbability)

  private val allValues: scala.collection.mutable.Set[V] = new scala.collection.mutable.HashSet[V]()
  def values: Set[V] = allValues.toSet
  private val filterMap: scala.collection.mutable.Buffer[(V,BloomFilter[K])]  = new scala.collection.mutable.ArrayBuffer[(V,BloomFilter[K])]()

  private def getFilter(value: V): Option[BloomFilter[K]] = filterMap.find(kv => kv._1 == value).map(_._2)
  private var keysAdded: Long = 0

  def add(key: K, value: V): Unit = {
    keysAdded += 1
    filterMap.find(kv => kv._1 == value).fold({
      val bf = newBloomFilter
      bf.put(key)
      filterMap += (value -> bf)
      ()
    })(kv => {
      kv._2.put(key)
      ()
    })
  }

  def fpp = filterMap.map(kv => kv._2.expectedFpp).max

  def get(key: K): List[V] = filterMap.flatMap(kv => {
    if (kv._2.mightContain(key)) {
      List(kv._1)
    } else {
      List()
    }
  }).toList
}
