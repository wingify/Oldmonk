package com.vwo.oldmonk.datastructures

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import Prop._

import scalaz._
import Scalaz._

class LensUtilsSpec extends Properties("LensUtilsSpec") {

  case class Foo(a: String, b: String)
  def getA(f: Foo) = new String(f.a)
  def setA(f: Foo, s: String) = f.copy(a=s)
  def cacheKey(f: Foo) = f.a

  property("cachedLens, basic lens properties") = forAll( (a: String, b: String, newA: String) => {
    val f = Foo(a,b)
    val lens = LensUtils.cachedLens(setA _, getA _, cacheKey _)

    val newF = lens.set(f, newA)

    ((lens.get(f) === a) :| "Lens returns correct value for get") &&
    ((lens.get(newF) === newA) :| "returns correct value for set")
  })

  property("cachedLens") = forAll( (a: String, b: String) => {
    val f = Foo(a,b)
    var getCount = 0
    val lens = LensUtils.cachedLens(setA _, (f: Foo) => {
      getCount += 1
      getA(f)
    }, cacheKey _)

    lens.get(f)

    ((lens.get(f) === a) :| "Lens returns correct value") &&
    ((getCount === 1) :| "get is called only once")
  })

  property("singletonCachedLens, basic lens properties") = forAll( (a: String, b: String, newA: String) => {
    val f = Foo(a,b)
    val lens = LensUtils.singletonCachedLens(setA _, getA _, cacheKey _)

    val newF = lens.set(f, newA)

    ((lens.get(f) === a) :| "Lens returns correct value for get") &&
    ((lens.get(newF) === newA) :| "returns correct value for set")
  })

  property("singletonCachedLens uses cache") = forAll( (a: String, b: String) => {
    val f = Foo(a,b)
    var getCount = 0
    val lens = LensUtils.singletonCachedLens(setA _, (f: Foo) => {
      getCount += 1
      getA(f)
    }, cacheKey _)

    val getResult = lens.get(f)

    ((lens.get(f) === a) :| "Lens returns correct value") &&
    ((lens.get(f) eq getResult) :| "Lens returns same object") &&
    ((getCount === 1) :| "get is called only once")
  })

  property("singletonCachedLens updates cache on set") = forAll( (a: String, b: String, newA: String) => {
    val f = Foo(a,b)
    var getCount = 0

    val lens = LensUtils.singletonCachedLens(setA _, (f: Foo) => {
      getCount += 1
      getA(f)
    }, cacheKey _)

    val newF = lens.set(f, newA)
    getCount = 0 //Get is apparently called at least once by set, but should not be called after this.

    ((lens.get(newF) === newA) :| "Lens returns correct value") &&
    ((lens.get(newF) eq newA) :| "Lens returns same object") &&
    ((getCount === 0) :| "get should not be called, but was called " + getCount + " times")
  })
}
