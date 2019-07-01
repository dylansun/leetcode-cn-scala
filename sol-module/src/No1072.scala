/**
  * Created by lilisun on 6/3/19.
  */
object No1072 {
  // 1 0 1
  // 0 1 0

  def maxEqualRowsAfterFlips(matrix: Array[Array[Int]]): Int = {
    matrix.map(_.toList).map{case h::t => t.map(_ ^ h)}.groupBy(x => x).values.map(_.length).max
  }
}
