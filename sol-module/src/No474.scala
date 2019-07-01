/**
  * Created by lilisun on 5/3/19.
  */
object No474 {
  def findMaxForm(strs: Array[String], m: Int, n: Int): Int = {
    val items = strs map countStr
    val dp = Array.fill(items.length + 1, m+1, n + 1)(0)
    for {
      i <- 1 to items.length
      j <- 0 to m
      k <- 0 to n
    }{

      if(items(i-1)._1 > j || items(i-1)._2 > k) dp(i)(j)(k) = dp(i-1)(j)(k)
      else dp(i)(j)(k) = dp(i-1)(j)(k) max (dp(i-1)(j-items(i-1)._1)(k - items(i-1)._2) + 1)
    }
    dp(items.length)(m)(n)
  }
  def countStr(s:String):(Int, Int) = countStr(s.toList,(0,0))
  def countStr(s:List[Char], acc:(Int,Int)):(Int,Int) = s match {
    case Nil => acc
    case '0'::t => countStr(t, (acc._1 + 1, acc._2))
    case '1'::t => countStr(t, (acc._1, acc._2 + 1))
  }

  def test():Unit = {
    val items = Array("0", "1", "10")
    val m = 1
    val n = 1
    println(findMaxForm(items, m, n))
  }
  // 1 dim 01 backpack problem example
  def example():Unit = {
    val w = Array(0 , 2 , 3 , 4 , 5 )		//商品的体积2、3、4、5
    val v = Array(0 , 3 , 4 , 5 , 6 )		//商品的价值3、4、5、6
    val bagV = 8					        //背包大小
    val dp = Array.fill(w.length, bagV+1)(0)			        //动态规划表

    for {
      i <- 1 until w.length
      j <- 1 to bagV
    }{
      if (j < w(i)) dp(i)(j) = dp(i-1)(j)
      else dp(i)(j) =dp(i-1)(j) max (dp(i - 1)(j - w(i)) + v(i))

      println(s"---------$i----$j------------")
      dp.foreach(x => println(x.toList))
    }
  }
  def main(args: Array[String]): Unit = {
    test()
  }
}
