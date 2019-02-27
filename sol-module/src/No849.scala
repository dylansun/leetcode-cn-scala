object No849 {
  def maxDistToClosest(seats: Array[Int]): Int = {
    val posOfOne = seats.zipWithIndex.filter(_._1 == 1)
    val mMid = (posOfOne zip posOfOne.tail).foldLeft(0)(
      (dis, x) => dis max (x._2._2 - x._1._2 ) / 2
    )
    disOfp(seats, 0) max disOfp(seats, seats.indices.last) max mMid
  }

  def d0(seats: Array[Int]) = seats.indexOf(1)

  def dn(seats: Array[Int]):Int =  seats.indices.last - seats.lastIndexOf(1)

  def disOfp(seats: Array[Int], p: Int): Int = {
    var l = 0
    var lf = true
    var r = 0
    var rf = true
    for(x <- (0 to p).reverse if lf ) if(seats(x) == 1) lf = false else l += 1
    if(p-l < 0) l = Int.MaxValue
    for(x <- p until seats.length if rf ) if(seats(x) == 1) rf = false else r += 1
    if(p+r >seats.length-1 ) r =Int.MaxValue
    l min r
  }
}
