/**
  * Created by lilisun on 3/11/19.
  */
object No697 {
  def findShortestSubArray(nums: Array[Int]): Int = {
    val d = nums
      .groupBy(x => x)
      .map(x => (x._1, x._2.length))
      .toList
      .sortBy(x => (- x._2, x._1))

     val candidate = d.filter(_._2 == d.map(_._2).max).map(_._1)

    var ans = nums.length
    candidate.foreach( x => ans = ans min (nums.lastIndexOf(x) - nums.indexOf(x) + 1))
    ans

  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1,2,2,3,1,4,2)
    nums.groupBy(x => x)
    println(nums.groupBy(x => x).map(x => (x._1, x._2.length)).toList.sortBy(x => (- x._2, x._1)))
  }

}
