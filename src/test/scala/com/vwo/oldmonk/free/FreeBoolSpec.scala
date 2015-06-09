package com.vwo.oldmonk.free

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import Prop._

import scalaz._
import Scalaz._

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

abstract class FreeBoolSpec(prop: String) extends Properties(prop) {

  type FreeBool[A]
  implicit val fbl: FreeBoolAlgebra[FreeBool]
  import fbl._

  property("lift and nat commute") = forAll( (x: Int) => {
    def f(t: Int) = (t % 3) == 1
    fbl.nat(f)(implicitly[Bool[Boolean]])(x.point[FreeBool]) == f(x)
  })

  property("lift and nat commute, free algebra") = forAll( (x: Int) => {
    def f(t: Int) = t.toString.point[FreeBool]

    fbl.nat(f)(fbl.bool[String])(x.point[FreeBool]) == f(x)
  })

  property("nat commutes with &") = forAll( (x: Int, y: Int) => {
    def f(t: Int) = t.toString.point[FreeBool]
    fbl.nat(f)(fbl.bool[String])(x.point[FreeBool] & y.point[FreeBool]) == (f(x) & f(y))
  })

  property("nat commutes with |") = forAll( (x: Int, y: Int) => {
    def f(t: Int) = t.toString.point[FreeBool]
    fbl.nat(f)(fbl.bool[String])(x.point[FreeBool] | y.point[FreeBool]) == (f(x) | f(y))
  })

  property("nat commutes with ~") = forAll( (x: Int) => {
    def f(t: Int) = t.toString.point[FreeBool]
    fbl.nat(f)(fbl.bool[String])(~x.point[FreeBool]) == (~f(x))
  })
}

class FreeBoolListSpec extends FreeBoolSpec("FreeBoolListSpec") {
  type FreeBool[A] = FreeBoolList[A]
  implicit val fbl = FreeBoolListAlgebra
}
