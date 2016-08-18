package com.vwo.oldmonk.datastructures

import com.google.common.hash._

trait FunnelUtils {
  trait ComposableFunnel[T] extends Funnel[T] { self =>
    def apply(value: T, sink: PrimitiveSink): PrimitiveSink

    override def funnel(t: T, sink: PrimitiveSink): Unit = apply(t, sink)

    def >\>[U](extractor: T => U)(implicit ufunnel: ComposableFunnel[U]) = new ComposableFunnel[T] {
      def apply(value: T, sink: PrimitiveSink): PrimitiveSink = ufunnel(extractor(value), self(value, sink))
    }

    def <*>[U](implicit ufunnel: ComposableFunnel[U]) = new ComposableFunnel[(T,U)] {
      def apply(value: (T,U), sink: PrimitiveSink) = ufunnel(value._2, self(value._1, sink))
    }

    def imap[U](f: U => T): ComposableFunnel[U] = new ComposableFunnel[U] {
      def apply(value: U, sink: PrimitiveSink) = self(f(value), sink)
    }
  }

  implicit class ProvidesToFunnel[T,U](f: T => U) {
    def toFunnel(implicit funnelU: ComposableFunnel[U]) = new ComposableFunnel[T] {
      def apply(t: T, sink: PrimitiveSink) = funnelU(f(t), sink)
    }
  }

  implicit def CharSequenceFunnel: Funnel[CharSequence] = Funnels.unencodedCharsFunnel()
  implicit def ByteArrayFunnel: Funnel[Array[Byte]] = Funnels.byteArrayFunnel()
  implicit def IntFunnel: Funnel[Int] = new ComposableFunnel[Int] {
    def apply(t: Int, sink: PrimitiveSink) = sink.putInt(t)
  }
  implicit def LongFunnel: ComposableFunnel[Long] = new ComposableFunnel[Long] {
    def apply(t: Long, sink: PrimitiveSink) = sink.putLong(t)
  }

  implicit def tuple2Funnel[A,B](implicit funnelA: ComposableFunnel[A], funnelB: ComposableFunnel[B]): ComposableFunnel[(A,B)] = funnelA <*> funnelB

  implicit def tuple3ComposableFunnel[A,B,C](implicit funnelA: ComposableFunnel[A], funnelB: ComposableFunnel[B], funnelC: ComposableFunnel[C]): ComposableFunnel[(A,B,C)] =
    new ComposableFunnel[(A,B,C)] {
      def apply(t: (A,B,C), sink: PrimitiveSink) = funnelA(t._1, funnelB(t._2, funnelC(t._3, sink)))
    }

  implicit def tuple4ComposableFunnel[A,B,C,D](implicit funnelA: ComposableFunnel[A], funnelB: ComposableFunnel[B], funnelC: ComposableFunnel[C], funnelD: ComposableFunnel[D]) =
    new ComposableFunnel[(A,B,C,D)] {
      def apply(t: (A,B,C,D), sink: PrimitiveSink) = funnelA(t._1, funnelB(t._2, funnelC(t._3, funnelD(t._4, sink))))
    }

  implicit def tuple5ComposableFunnel[A,B,C,D,E](implicit funnelA: ComposableFunnel[A], funnelB: ComposableFunnel[B], funnelC: ComposableFunnel[C], funnelD: ComposableFunnel[D], funnelE: ComposableFunnel[E]) =
    new ComposableFunnel[(A,B,C,D,E)] {
      def apply(t: (A,B,C,D,E), sink: PrimitiveSink) = funnelA(t._1, funnelB(t._2, funnelC(t._3, funnelD(t._4, funnelE(t._5,sink)))))
    }

  implicit object UUIDFunnel extends ComposableFunnel[java.util.UUID] {
    def apply(uuid: java.util.UUID, into: PrimitiveSink) = {
      into.putLong(uuid.getLeastSignificantBits())
        .putLong(uuid.getMostSignificantBits())
    }
  }

  implicit object BooleanFunnel extends Funnel[Boolean] {
    def funnel(b: Boolean, into: PrimitiveSink) = into.putBoolean(b)
  }

  implicit def OptionFunnel[T](implicit f: ComposableFunnel[T]) = new ComposableFunnel[Option[T]] {
    def apply(ot: Option[T], sink: PrimitiveSink) = ot.fold(sink.putInt(0))(t => f(t,sink))
  }
}
