/**
  * Created by lilisun on 2/17/19.
  */
object No350 {
  def intersect(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    val n1 = nums1.groupBy[Int](x => x)
    val n2 = nums2.groupBy[Int](x => x)
    val n3 = n1.keySet intersect n2.keySet
    var res = List[Int]()
    n3.foreach(x => {
      val n =n1(x).length min n2(x).length
      for(y <- 1 to n) res = x :: res
    })

    res.toArray
  }

  def main(args: Array[String]): Unit = {
    val a = Seq(4, 5, 6, 4, 2)
    val b: Map[Int, Seq[Int]] = a.groupBy[Int](x => x)
    println(s"b: $b")

  }

}
