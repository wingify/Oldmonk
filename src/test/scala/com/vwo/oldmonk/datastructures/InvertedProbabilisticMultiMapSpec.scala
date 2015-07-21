package com.vwo.oldmonk.datastructures

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import Prop._

import scalaz._
import Scalaz._

import com.google.common.hash.{Funnel, PrimitiveSink}

class InvertedProbabilisticMultiMapSpec extends Properties("InvertedProbabilisticMultiMap") {

  implicit val stringFunnel = new Funnel[String] {
    def funnel(data: String, into: PrimitiveSink) = into.putBytes(data.getBytes)
  }

  property("add and get") = forAll( (k: String, v: Long) => {
    val mp = new InvertedProbabilisticMultiMap[String,Long](1024, 1e-8)
    mp.add(k,v)
    mp.get(k).map(_ === v) === List(true)
  })

  property("add and get many") = forAll( (dataExtra: Map[String,Long]) => {
    val mp = new InvertedProbabilisticMultiMap[String,Long](1024, 1e-16)
    val data = dataExtra.toList.filter(kv => kv._1.size > 0)
    data.map(kv => mp.add(kv._1, kv._2) )
    data.forall(kv => {
      val getResult = mp.get(kv._1)
      if (getResult.size > 1) {
        println(getResult)
        println(data.filter(kv => getResult.toSet.contains(kv._2)))
      }
      (getResult.headOption === Some(kv._2)) && getResult.tail.isEmpty
    })
  })

  property("values not added are missing") = forAll( (k: String, k2: String, v: Long) => {
    val mp = new InvertedProbabilisticMultiMap[String,Long](1024*1024, 1e-8)
    mp.add(k,v)
    (k != k2) ==> mp.get(k2).isEmpty
  })

  property("values not added are missing many") = forAll( (data: Map[String,Long], missing: Set[String]) => {
    val mp = new InvertedProbabilisticMultiMap[String,Long](1024*1024, 1e-8)
    data.toList.map(kv => mp.add(kv._1, kv._2) )
    val reallyMissing = missing -- data.keySet // Need to remove
    reallyMissing.forall( k => mp.get(k).isEmpty)
  })



}
