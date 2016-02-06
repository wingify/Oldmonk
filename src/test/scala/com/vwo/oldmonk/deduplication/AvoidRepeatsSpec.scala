package com.vwo.oldmonk.deduplication

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import Prop._
import com.google.common.hash._

import scalaz._
import Scalaz._

abstract class IdempotentEffectGenericSpec(name: String) extends Properties(name) {
  def newEffect(f: String => Unit): IdempotentEffect[String]

  property("Dedupe effects") = forAll( (x: String) => {
    var cnt: Int = 0
    val f = newEffect((k:String) => {
      cnt = cnt + 1
    })
    f(x)
    f(x)
    cnt === 1
  })

  property("Dedupe effects pt 2") = forAll( (x: String, y: String) => {
    var cnt: Int = 0
    val f = newEffect((k:String) => {
      cnt = cnt + 1
    })
    f(x)
    f(x)
    f(y)
    cnt === Set(x,y).size
  })
}

class IdempotentEffectSpec extends IdempotentEffectGenericSpec("IdempotentEffectSpec") {
  def newEffect(f: String => Unit): IdempotentEffect[String] = IdempotentEffect(f)
}

class BloomFilterIdempotentEffectSpec extends IdempotentEffectGenericSpec("BloomFilterIdempotentEffectSpec") {
  implicit val stringFunnel = new Funnel[String] {
    def funnel(data: String, into: PrimitiveSink) = into.putBytes(data.getBytes)
  }

  def newEffect(f: String => Unit): IdempotentEffect[String] = new BloomFilterIdempotentEffect(f, insertionsBeforeRotate = 100)

  property("Dedupe works after rotate") = forAll( (x: String) => {
    var cnt: Int = 0
    val f = newEffect((k:String) => {
      cnt = cnt + 1
    })
    var i=0
    while (i < 150) {
      f(x + i)
      i += 1
    }
    i = 0
    while (i < 150) {
      f(x + i)
      i += 1
    }
    cnt === 150
  })

  property("Rotations occur, after 2 rotates dedupe should fail") = forAll( (x: String) => {
    var cnt: Int = 0
    val f = newEffect((k:String) => {
      cnt = cnt + 1
    })
    var i=0
    while (i < 300) {
      f(x + i)
      i += 1
    }
    i = 0
    while (i < 100) {
      f(x + i)
      i += 1
    }
    cnt === 400
  })
}

class DelayedIdempotentEffectSpec extends Properties("DelayedIdempotentEffectSpec") {
  property("Dedupe delayed effects") = forAll( (x: String, xx: Long, xxx: Long, xxxx: Long) => {
    var cnt: Int = 0
    val f = DelayedIdempotentEffect((k:String, other: Long) => {
      cnt = cnt + 1
    })
    val marker = f(x, xx)
    f(x, xxx)
    marker.foreach(x => x())
    f(x, xxxx)
    ((marker.map(_.key) === Some(x)) :| "Key on marker is correct") &&
    ((cnt === 2) :| "Effect happened before marker used, never after")
  })

  property("Dedupe delayed effects 2") = forAll( (x: String, xx: Long, y: String, yy: Long) => {
    var cnt: Int = 0
    val f = DelayedIdempotentEffect((k:String, other: Long) => {
      cnt = cnt + 1
    })
    val marker = f(x, xx)
    marker.foreach(x => x())
    f(y, yy).map(x => x())
    ((marker.map(_.key) === Some(x)) :| "Key on marker is correct") &&
    ((cnt === Set(x,y).size) :| "Effect happened before marker used, never after")
  })
  property("Dedupe delayed returns None when previously run") = forAll( (x: String, xx: Long, y: String, yy: Long) => {
    var cnt: Int = 0
    val f = DelayedIdempotentEffect((k:String, other: Long) => {
      cnt = cnt + 1
    })
    val marker1 = f(x, xx)
    marker1.foreach(x => x())
    val marker2 = f(x, yy)
    marker2.foreach(x => x())
    val marker1Again = f(x, yy)
    ((marker1.isDefined :| "Marker1 should be defined") && (marker1Again.isEmpty :| "Marker1Again should be empty"))
  })
}
