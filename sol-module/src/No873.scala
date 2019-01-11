/**
  * Created by lilisun on 1/12/19.
  */
object No873 {
  def lenLongestFibSubseq(A: Array[Int]): Int = {
    val n = A.length
    val S = A.toSet
    var ans = 0
    for(i <- 0 to n -1){
      for(j <- i+1 to n-1){
        val tmp = lxy(A(i), A(j), S)
        if(ans < tmp ) ans = tmp
      }
    }
    return ans
  }

  def lxy(x: Int, y: Int, s: Set[Int]): Int = {
    var a0 = x
    var a1 = y
    var a2 = x + y
    var len = 2
    while(s.contains(a2)){
      a0 = a1
      a1 = a2
      a2 = a0 + a1
      len = len + 1
    }
    return if(len >= 3) len else 0
  }

  def main(args: Array[String]): Unit = {
    val s = Set[Int](1, 2, 3, 4, 5, 7, 8)
    println(lxy(1, 2, s))
  }
}
