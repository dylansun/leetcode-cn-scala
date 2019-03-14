/**
  * Created by lilisun on 3/5/19.
  */
object No960 {
  def minDeletionSize(A: Array[String]): Int = {
    val num = A.length
    val dp = Array.fill(A(0).length)(1)
    for(i <- A(0).indices)
      for(j <- 0 until i)
        if(A.forall( x=> x(j) <= x(i)))
          dp(i) = dp(i) max  (dp(j) + 1)
    return A(0).length - dp.max
  }
}
