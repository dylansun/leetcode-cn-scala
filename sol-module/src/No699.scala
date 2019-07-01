/**
  * Created by lilisun on 5/13/19.
  */
object No699 {
  object OfflinePropagation{
    def fallingSquares(positions: Array[Array[Int]]): List[Int] = {
      val ans = Array.fill(positions.length)(0)
      for{
        i <- positions.indices
        left = positions(i)(0)
        size = positions(i)(1)
        right = left + size
      }{
        ans(i) += size
        for{
          j <- i+1 to positions.indices.last
          left2 = positions(j)(0)
          size2 = positions(j)(1)
          right2 = left2 + size2
          if left2 < right && left < right2
        }
          ans(j) = ans(i) max ans(j) // ans(j) may be bigger
      }
      var max_height = 0
      (for{x <- ans} yield{max_height = max_height max x; max_height}).toList
    }
  }
}
