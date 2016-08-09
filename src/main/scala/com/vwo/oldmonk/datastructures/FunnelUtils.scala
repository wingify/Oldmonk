package com.vwo.oldmonk.datastructures

import com.google.common.hash._

trait FunnelUtils {
  implicit def CharSequenceFunnel: Funnel[CharSequence] = Funnels.unencodedCharsFunnel()
  implicit def ByteArrayFunnel: Funnel[Array[Byte]] = Funnels.byteArrayFunnel()
  implicit def IntFunnel: Funnel[Int] = new Funnel[Int] {
    def funnel(t: Int, sink: PrimitiveSink) = sink.putInt(t)
  }
  implicit def LongFunnel: Funnel[Long] = new Funnel[Long] {
    def funnel(t: Long, sink: PrimitiveSink) = sink.putLong(t)
  }

  implicit def tuple2Funnel[A,B](implicit funnelA: Funnel[A], funnelB: Funnel[B]) = new Funnel[(A,B)] {
    def	funnel(from: (A,B), into: PrimitiveSink) = {
      funnelA.funnel(from._1, into)
      funnelB.funnel(from._2, into)
    }
  }

  implicit def tuple3Funnel[A,B,C](implicit funnelA: Funnel[A], funnelB: Funnel[B], funnelC: Funnel[C]) = new Funnel[(A,B,C)] {
    def	funnel(from: (A,B,C), into: PrimitiveSink) = {
      funnelA.funnel(from._1, into)
      funnelB.funnel(from._2, into)
      funnelC.funnel(from._3, into)
    }
  }

  implicit def tuple4Funnel[A,B,C,D](implicit funnelA: Funnel[A], funnelB: Funnel[B], funnelC: Funnel[C], funnelD: Funnel[D]) = new Funnel[(A,B,C,D)] {
    def	funnel(from: (A,B,C,D), into: PrimitiveSink) = {
      funnelA.funnel(from._1, into)
      funnelB.funnel(from._2, into)
      funnelC.funnel(from._3, into)
      funnelD.funnel(from._4, into)
    }
  }

  implicit def tuple5Funnel[A,B,C,D,E](implicit funnelA: Funnel[A], funnelB: Funnel[B], funnelC: Funnel[C], funnelD: Funnel[D], funnelE: Funnel[E]) = new Funnel[(A,B,C,D,E)] {
    def	funnel(from: (A,B,C,D,E), into: PrimitiveSink) = {
      funnelA.funnel(from._1, into)
      funnelB.funnel(from._2, into)
      funnelC.funnel(from._3, into)
      funnelD.funnel(from._4, into)
      funnelE.funnel(from._5, into)
    }
  }

  implicit object UUIDFunnel extends Funnel[java.util.UUID] {
    def funnel(uuid: java.util.UUID, into: PrimitiveSink) = {
      into.putLong(uuid.getLeastSignificantBits())
      into.putLong(uuid.getMostSignificantBits())
    }
  }

  implicit object BooleanFunnel extends Funnel[Boolean] {
    def funnel(b: Boolean, into: PrimitiveSink) = into.putBoolean(b)
  }
}
