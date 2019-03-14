/**
  * Created by lilisun on 3/14/19.
  */
object No956 {


//https://leetcode.com/problems/tallest-billboard/discuss/203181/JavaC++Python-DP-min(O(SN2)-O(3N2-*-N)
  def tallestBillboard(rods: Array[Int]): Int = {
    val dp = Array.fill(5001)(0)
    for ( d <- 1 until 5001) dp(d) = -10000

    for ( x <- rods) {
      val cur = dp.clone()
      var d= 0
      while ( d + x < 5001) {
        dp(d + x) = dp(d + x) max cur(d)
        dp(Math.abs(d - x)) = dp(Math.abs(d - x)) max (cur(d) + (d min x))
        d += 1
      }
    }
    dp(0)
  }


  // TLE
  def tallestBillboard_bf(rods: Array[Int]): Int = {
    val m = scala.collection.mutable.HashMap[Int, List[Int]]()
    for(i <- 0 until Math.pow(2, rods.length).toInt){
      val key = bitsSum(rods, i)
      if(key >= 5000) println(key)
      m.put(key, i::m.getOrElse(key, Nil))
    }

    println(m.size)
    val m2 = m.filter(_._2.length > 1)
    println(m2.size)
    m2
      .filter(x => isNonIntersect(x._2))
      .keySet
      .toList
      .sortBy(x => -x)
      .headOption
      .getOrElse(0)

  }

  def isNonIntersect(l: List[Int]):Boolean = {
    l.exists(x => l.exists(y => (y & x) == 0))
  }

  def bitsSum(rods: Array[Int], bits: Int): Int ={
    var t: Int = bits
    var sum = 0
    for(j <- rods.indices){
      if((t & 1) == 1) sum += rods(j)
      t >>=  1
    }
    sum
  }

  def main(args: Array[String]): Unit = {
    println(Math.pow(2, 19).toInt, Int.MaxValue)
    println(tallestBillboard(Array(1,2,4,8,16,32,64,128,256,512,50,50,50,150,150,150,100,100,100,123)))
    println(tallestBillboard(Array(1,2)))
    println(tallestBillboard(Array(227,259,250,262,217,280,243,228,244,269,253,228,262,273,240,253,270,242)))
  }
}
