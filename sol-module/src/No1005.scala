/**
  * Created by lilisun on 3/10/19.
  */
object No1005 {
  def largestSumAfterKNegations(A: Array[Int], k: Int): Int = {
    val ps = A.filter(_ < 0)
    val pb = A.filter(_ >= 0)
    val min_abs = A.map(x=> Math.abs(x)).min
    if(k >= ps.length){
      (k - ps.length) % 2 match {
        case 0 => A.map(x => Math.abs(x)).sum
        case 1 => A.map(x => Math.abs(x)).sum - 2 * min_abs
      }
    }else{
      ps
        .sorted
        .zipWithIndex
        .map( t => if(t._2 < k) -t._1 else t._1)
        .sum + pb.sum
    }
  }

  def main(args: Array[String]): Unit = {
    val A = Array(3,-1,0,2)
    val k = 3
    println(largestSumAfterKNegations(A, 3))
  }
}
