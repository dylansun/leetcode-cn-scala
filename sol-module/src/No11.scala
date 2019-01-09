/**
  * Created by lilisun on 1/10/19.
  */
object No11 {
  def maxArea(height: Array[Int]): Int = {
    var maxarea = 0
    for (i <- 0 to height.length - 1)
      for (j <- i + 1 to  height.length -1)
         maxarea = maxarea max (height(i) min height(j)) * (j - i)
    return maxarea
  }
}
