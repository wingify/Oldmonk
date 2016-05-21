package com.vwo.oldmonk.datastructures

import com.google.common.hash._
import com.google.common.base.Predicate

import java.util.concurrent.atomic._
import scala.collection.JavaConversions._

class SemiConcurrentBloomFilter[A](filterSize: Int = 1024*1024, falsePositiveProbability: Double = 1e-12, insertionsBeforeCompact: Int = 1024)(implicit funnel: Funnel[A]) {
  /*
   * A semi-bloomfilter like datastructure, but which should be safe to use in concurrent scenarios.
   *
   * Unlike a bloomfilter this does not necessarily have bounded space requirements.
   * Ideally space requirements should be O(insertionsBeforeCompact), but I have no proof
   * of this fact.
   */
  private val newValues = new java.util.concurrent.ConcurrentHashMap[A,A]()

  private val mainFilter: AtomicReference[BloomFilter[A]] = new AtomicReference(
    BloomFilter.create[A](funnel, filterSize, falsePositiveProbability)
  )

  private val insertionsSinceCompact = new AtomicInteger(0)

  //Statistics
  private val attemptedCompactions = new AtomicLong(0)
  def numAttemptedCompactions = attemptedCompactions.get()
  private val successfulCompactions = new AtomicLong(0)
  def numSuccessfulCompactions = successfulCompactions.get()

  def add(a: A) = {
    val bf = mainFilter.get()
    if (!bf.mightContain(a) ) {
      newValues.put(a,a)
      val numInserted = insertionsSinceCompact.incrementAndGet()
      if (numInserted % insertionsBeforeCompact == 0) {
        maybeCompact
      }
    }
  }

  // After this operation is called memory usage should go down.
  def maybeCompact: Unit = {
    attemptedCompactions.incrementAndGet()
    val size = newValues.size()
    val toAdd: Seq[A] = newValues.keys().take(size).toSeq //We restrict the size to ensure that we don't sit in an infinite loop here
    val oldBf = mainFilter.get()
    val newBf = oldBf.copy()
    toAdd.foreach(newBf.put _)
    val addedNew: Boolean = mainFilter.compareAndSet(oldBf, newBf)
    if (addedNew) { //If this succeeds, then both mainFilter and newValues contain all elements of toAdd
      toAdd.foreach(newValues.remove _) //Now it's safe to remove from newValues
      insertionsSinceCompact.addAndGet(-1*size) //Finally reduce insertions
      successfulCompactions.incrementAndGet() //Statistics
    }
  }

  def mightContain(a: A) = {
    val fromNew = newValues.get(a) //If a has been removed from newValues
    if (fromNew != null) {
      true
    } else { //Then we have already added
      mainFilter.get().mightContain(a)
    }
  }
}
