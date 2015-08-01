package com.vwo.oldmonk.diffs

trait ProvidesLongestCommonSubsequence {

  trait LongestCommonSubsequence[T] {
    def left: Array[T]
    def right: Array[T]
    def indices: List[(Int,Int)]
    def toSeq: Seq[T] = indices.map(x => left(x._1))
    def toChunks: List[(Range,Range)] = indices.foldLeft[List[(Range,Range)]](Nil)(
      (lst, nxt) => lst.headOption.fold(
        List((Range(nxt._1, nxt._1+1), Range(nxt._2, nxt._2+1)))
      )(h => {
        if ((h._1.end == nxt._1) && (h._2.end == nxt._2)) {
          ((Range(h._1.start, nxt._1+1), Range(h._2.start, nxt._2+1) )) :: lst.tail
        } else {
          ((Range(nxt._1, nxt._1+1), Range(nxt._2, nxt._2+1) )) :: lst
        }
      })
    ).reverse
  }

  private case class LongestCommonSubsequenceImpl[T](left: Array[T], right: Array[T], indices: List[(Int,Int)]) extends LongestCommonSubsequence[T]

  def longestCommonSubsequence[T](a: List[T], b: List[T])(implicit ct: scala.reflect.ClassTag[T]): LongestCommonSubsequence[T] = longestCommonSubsequence(a.toArray, b.toArray)

  def longestCommonSubsequence[T](a: Array[T], b: Array[T]): LongestCommonSubsequence[T] = {
    val lengths: Array[Array[Long]] = (1 to a.length + 1).map(_ => new Array[Long](b.length+1)).toArray

    // row 0 and column 0 are initialized to 0 already

    var i: Int = 0
    while (i < a.size) {
      var j: Int = 0
      while (j < b.size) {
        if (a(i) == b(j)) {
          lengths(i+1)(j+1) = lengths(i)(j) + 1
        } else {
          lengths(i+1)(j+1) = java.lang.Math.max(lengths(i+1)(j), lengths(i)(j+1))
        }
        j += 1
      }
      i += 1
    }

    // read the substring out from the matrix
    var sb: List[(Int,Int)] = Nil
    var x = a.length
    var y = b.length
    while ((x != 0) && (y != 0)) {
      if (lengths(x)(y) == lengths(x-1)(y)) {
        x -= 1
      } else if (lengths(x)(y) == lengths(x)(y-1)) {
        y -= 1
      } else {
        require(a(x-1) == b(y-1))
        sb = (x-1, y-1) :: sb
        x -= 1
        y -= 1
      }
    }

    LongestCommonSubsequenceImpl(a, b, sb)
  }
}
