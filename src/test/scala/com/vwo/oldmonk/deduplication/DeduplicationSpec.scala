package com.vwo.oldmonk.deduplication

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import Prop._

import scalaz._
import Scalaz._

class DeduplicationSpec extends Properties("Deduplication") {
  property("Deduplicator") = forAll( (x: List[String], y: List[String]) => {
    val f = Deduplicator[List[String]]()
    (f(x) eq x) && (f(x) eq f(f(x))) && (f(y) eq y) // Use 'eq' to check reference equality, i.e. both are same object
  })
}
