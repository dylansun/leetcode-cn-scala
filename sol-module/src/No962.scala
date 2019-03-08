/**
  * Created by lilisun on 3/8/19.
  */
object No962 {
  def maxWidthRamp(A: Array[Int]): Int = {
    val candidate = (for(i <- 0 until A.length) yield {
      for(j <- i+1 until A.length if A(i) <= A(j)) yield  j-i
    }).flatten
    if(candidate.isEmpty) 0 else candidate.max

  }

  def solver_vf(A: Array[Int]):Int = {
    var i = A.length - 1
    while (i > 0) {
      var left = 0
      var right = i

      while (right < A.length) {
        if (A(left) <= A(right)) {
          return right - left
        } else {
          left+=1
          right+=1
        }
      }

      i-=1
    }
    0
  }
}
