object No875 {
  def minEatingSpeed(piles: Array[Int], H: Int): Int = {
    var lo = 1; var hi = 1000000000 min piles.max
    while ( lo < hi){
      val mi = (lo + hi) / 2
      if(!possible(piles, H, mi))
        lo = mi + 1
      else
        hi = mi
    }
    return lo
  }

  // Can Koko eat all bananas in H hours with eating speed K?

  def possible(piles: Array[Int], H: Int, K:Int): Boolean = {
    var time = 0
    for (p <- piles)
    time += (p-1) / K + 1
    return time <= H
  }
}

