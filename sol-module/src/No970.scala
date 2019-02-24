/**
  * Created by lilisun on 2/24/19.
  */
object No970 {
  def powerfulIntegers(x: Int, y: Int, bound: Int): List[Int] = {
    if(bound < 2) return List[Int]()
    if(x == 1 && y == 1) List[Int](2)
    else if(x == 1 && y > 1)
      (for(i <- 0 to maxpower(y, bound - 1)) yield 1 + Math.pow(y, i).toInt).toList
    else if(y == 1 && x > 1)
      (for(i <- 0 to maxpower(x, bound - 1)) yield 1 + Math.pow(x, i).toInt).toList
    else {
      (for(i <- 0 to maxpower(x, bound-1)) yield{
        for(j <- 0 to maxpower(y, bound-1)) yield (Math.pow(x,i) + Math.pow(y,j)).toInt
      }).flatten.toList.distinct.filter(_ <= bound)
    }
  }

  def maxpower(base: Int, bound: Int, ans: Int =0):Int =if(bound >= base ) maxpower(base, bound / base, ans + 1) else ans

  def main(args: Array[String]): Unit = {
    println(powerfulIntegers(2,3,10))
  }
}
