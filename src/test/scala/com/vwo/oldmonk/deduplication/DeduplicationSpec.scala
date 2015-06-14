package com.vwo.oldmonk.deduplication

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import Prop._

import scalaz._
import Scalaz._

class DeduplicationSpec extends Properties("Deduplication") {

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

  property("Deduplicator") = forAll( (x: List[String], y: List[String]) => {
    val f = Deduplicator[List[String]]()
    (f(x) eq x) && (f(x) eq f(f(x))) && (f(y) eq y) // Use 'eq' to check reference equality, i.e. both are same object
  })
}
