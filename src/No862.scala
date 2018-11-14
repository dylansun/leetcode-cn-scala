/**
  * Created by lilisun on 11/14/18.
  */
object No862 {
  def shortestSubarray(A: Array[Int], K: Int): Int = {
    val t = (for(i <- A.indices if A(i) > 0 ) yield f(i, A, K))
    if(t.exists(_>0)) return t.filter(_>0).min
    else -1
  }
  def f(start: Int, A: Array[Int], K: Int): Int = {
    var count = 0
    var t = K
    for(i <- start to A.length - 1){
      t = t - A(i)
      count = count + 1
      if(t <= 0){
        return count
      }
    }

    return -1

  }

  def main(args: Array[String]): Unit = {

    val A = Array(2,-1,2,2)
    val K = 3
    println(shortestSubarray(A, K))
  }

}
