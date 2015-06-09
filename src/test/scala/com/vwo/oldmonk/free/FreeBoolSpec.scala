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

object FreeBoolSpec extends Properties("FreeBoolSpec") {
//  implicit val fb = FreeBool.FreeBoolBooleanAlgebra[Int]

  implicit val fbl = FreeBoolListAlgebra
  import fbl._

  property("lift and nat commute") = forAll( (x: Int) => {
    def f(t: Int) = (t % 3) == 1

    fbl.nat(f)(implicitly[Bool[Boolean]])(x.point[FreeBoolList]) == f(x)
  })

  property("lift and nat commute, free algebra") = forAll( (x: Int) => {
    def f(t: Int) = t.toString.point[FreeBoolList]

    fbl.nat(f)(fbl.bool[String])(x.point[FreeBoolList]) == f(x)
  })

  property("nat commutes with &") = forAll( (x: Int, y: Int) => {
    def f(t: Int) = t.toString.point[FreeBoolList]
    fbl.nat(f)(fbl.bool[String])(x.point[FreeBoolList] & y.point[FreeBoolList]) == (f(x) & f(y))
  })

  property("nat commutes with |") = forAll( (x: Int, y: Int) => {
    def f(t: Int) = t.toString.point[FreeBoolList]
    fbl.nat(f)(fbl.bool[String])(x.point[FreeBoolList] | y.point[FreeBoolList]) == (f(x) | f(y))
  })

  property("nat commutes with ~") = forAll( (x: Int) => {
    def f(t: Int) = t.toString.point[FreeBoolList]
    fbl.nat(f)(fbl.bool[String])(~x.point[FreeBoolList]) == (~f(x))
  })
}
