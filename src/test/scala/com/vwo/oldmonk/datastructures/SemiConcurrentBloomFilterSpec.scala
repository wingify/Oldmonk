package com.vwo.oldmonk.datastructures

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import Prop._

import scalaz._
import Scalaz._

import scala.collection.parallel.immutable.ParVector
import com.google.common.hash.{Funnel, PrimitiveSink}

class SemiConcurrentBloomFilterSpec extends Properties("SemiConcurrentBloomFilter") {

  property("added elements are present") = forAll( (data: Set[Long]) => {
    val bf = new SemiConcurrentBloomFilter[Long](insertionsBeforeCompact = 4) // Chosen to ensure lots of contention
    data.par.foreach(bf.add _)

    data.par.map(bf.mightContain _).forall(x => x)
  })
  property("non-added elements are absent") = forAll( (data: Set[Long], otherData: Set[Long]) => {
    val dataToAdd = data -- otherData
    val bf = new SemiConcurrentBloomFilter[Long](insertionsBeforeCompact = 4) // Chosen to ensure lots of contention
    dataToAdd.par.foreach(bf.add _)

    dataToAdd.map(bf.mightContain _).forall(x => x) && otherData.map(bf.mightContain _).forall(x => !x)
  })
}
