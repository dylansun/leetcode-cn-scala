/**
  * Created by lilisun on 2/15/19.
  */
import collection.mutable.ArrayBuffer
import scala.io.Source
import util.control.Breaks._
object No164 {
  def maximumGap(nums: Array[Int]): Int = {
    val time1 = System.currentTimeMillis()
    if (nums.length < 2) return 0
    if (nums.max == nums.min) return 0
    //Initial buckets
    val n = nums.length - 1
    val bGap = (nums.max - nums.min) / n.toDouble
    var buckets = List[ArrayBuffer[Int]]()
    for (x <- 0 to n + 1) buckets = ArrayBuffer[Int]() :: buckets //((i-1)*bGap, i * bGap)

    val time2 = System.currentTimeMillis()
    println(s"initial time: ${time2 - time1}")
    //put every elements to buckets
    for (x <- nums) {
      if (x == nums.min) buckets(0) += x
      else if (x == nums.max) buckets(n + 1) += x
      else {
        val t = ((x - nums.min) / bGap).toInt + 1 // ???
        buckets(t) += x
      }
    }
    val time3 = System.currentTimeMillis()
    println(s"putting time: ${time3 - time2}")
    var max_gap = bGap

    //println(s"n: $n")
    //for(x <- 0 to n+1 ){
    //  println(s"$x: ${buckets(x).mkString}")
    //}

    for (x <- 1 to n) {
      //find the empty
      if (buckets(x).isEmpty) {
        //println(s"found empty bucket: $x")

        //left valid
        var left_valid = 0
        for (y <- (0 until x).reverse) {
          breakable {
            if (buckets(y).nonEmpty) {
              left_valid = left_valid max y
              break()
            }
          }
        }


        var right_valid = n + 1
        for (y <- x + 1 to n + 1) {
          breakable {
            if (buckets(y).nonEmpty) {
              right_valid = right_valid min y
              break()
            }
          }
        }
        //println(s"right valid buckets: $right_valid")
        max_gap = max_gap max (buckets(right_valid).min - buckets(left_valid).max)
      }
    }
    val time4 = System.currentTimeMillis()
    println(s"calculate time: ${time4 - time3}")
    max_gap.toInt
  }



  def maximumGap2(nums: Array[Int]): Int = {

      if(nums.length<2){
        return 0
      }
      nums.sorted
      var sum=0
      var i=1
      while(i<nums.length){
        val sum1=nums(i)-nums(i-1)
        if(sum1>sum){
          sum=sum1
        }else{
          i += 1
        }
      }
      return sum
    }

  def main(args: Array[String]): Unit = {
    val t = Array(1,2,3,4,6)

    var t2 = ArrayBuffer[Int]()
    val file=Source.fromFile("sol-module/testcases/No164/testcase1.txt")

    for(line <- file.getLines)
    {
      val line_array = line.split(",")
      for(x <- line_array) t2 += x.toInt
    }
    file.close
    println(s"t2: ${t2.size}")

    println(maximumGap2(t2.toArray))
  }
}
