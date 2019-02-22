/**
  * Created by lilisun on 2/23/19.
  */
object No48 {
  def rotate(matrix: Array[Array[Int]]): Unit = {
    val len = matrix.length
    for {
      layer <- 0 until len / 2
      (topRow, rightColumn, downRow, leftColumn) = (layer, len - 1 - layer, len - 1 - layer, layer)
      i <- layer until len - 1-layer
    } {
      val (top, right, down, left) = (matrix(topRow)(i), matrix(i)(rightColumn), matrix(downRow)(len-i-1), matrix(len-1-i)(leftColumn))
      matrix(topRow)(i) = left
      matrix(i)(rightColumn) = top
      matrix(downRow)(len -  i - 1) = right
      matrix(len  - i - 1)(leftColumn) = down
    }
  }
}
