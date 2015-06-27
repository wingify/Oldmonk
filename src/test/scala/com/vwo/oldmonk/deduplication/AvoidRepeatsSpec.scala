package com.vwo.oldmonk.deduplication

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import Prop._

import scalaz._
import Scalaz._

class AvoidRepeatsSpec extends Properties("AvoidRepeats") {

  property("Dedupe effects") = forAll( (x: String) => {
    var cnt: Int = 0
    val f = IdempotentEffect((k:String) => {
      cnt = cnt + 1
    })
    f(x)
    f(x)
    cnt === 1
  })

  property("Dedupe effects pt 2") = forAll( (x: String, y: String) => {
    var cnt: Int = 0
    val f = IdempotentEffect((k:String) => {
      cnt = cnt + 1
    })
    f(x)
    f(x)
    f(y)
    cnt === Set(x,y).size
  })

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
}
