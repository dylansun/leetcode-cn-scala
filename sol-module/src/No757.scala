
object No757 {

  def intersectionSizeTwo(intervals: Array[Array[Int]]): Int = {
    var ans = 0; var p1 = -1; var p2 = -1
    val itv = sortintervals(intervals)
    itv.foreach( x=>{
      if(x(0) > p1){
        if(x(0) > p2){
          ans += 2
          p2 = x(1)
          p1 = p2 -1
        }
        else{
          ans += 1
          p1 = p2
          p2 = x(1)
        }
      }
      println(s"ans: $ans, p1: $p1, p2: $p2")
    })
    ans
  }

  def sortintervals(interval: Array[Array[Int]]): Array[Array[Int]] = interval.sortBy(x => (x(1), -x(0)))

  def main(args: Array[String]): Unit = {
    val t1 = Array(Array(1,3), Array(1,4), Array(2,5), Array(3,5))
    println(intersectionSizeTwo(t1))
  }
}
