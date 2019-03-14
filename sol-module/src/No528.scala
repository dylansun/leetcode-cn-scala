/**
  * Created by lilisun on 3/15/19.
  */
object No528 {
  class Solution(_w: Array[Int]) {

    val sums = Array.fill(_w.length)(0)
    for (i <- _w.indices) i match {
      case 0 => sums(0) = _w(0)
      case _ => sums(i) = sums(i-1) + _w(i)
    }

    def pickIndex(): Int = {
      binarySearch(scala.util.Random.nextInt(sums.last))
    }

    def binarySearch(target: Int):Int ={
      var lo = 0
      var hi = sums.length - 1
      while (lo <= hi) {
        val mid = (lo + hi) / 2
        if (sums(mid) <= target) {
          lo = mid + 1
        } else {
          hi = mid - 1
        }
      }
      lo
    }
  }

}
