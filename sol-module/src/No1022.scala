/**
  * Created by lilisun on 3/24/19.
  */
object No1022 {
  def maxScoreSightseeingPair(A: Array[Int]): Int = {
    var ans =0
    for(i <- 0 until  A.length)
      for (j <- i+1 until (i+2000 min A.length) if A(i)+A(j)+i-j > ans)
        ans = A(i)+A(j)+i-j
    ans
  }
}
