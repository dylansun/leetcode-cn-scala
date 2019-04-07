/**
  * Created by lilisun on 4/5/19.
  */
object No906 {
  def superpalindromesInRange(L: String, R: String): Int = {
    var ans = 0
    for{ i <- 1 to Math.pow(10, (R.length  + 1) / 2).toInt
         if isPal(i.toString)
         if condition(BigInt(i)*BigInt(i), BigInt(L))
    } {
      if(BigInt(i) * BigInt(i) > BigInt(R)){
        return ans
      }
      println(i, BigInt(i) * BigInt(i))
      ans += 1
    }
    ans
  }
  def condition(n:BigInt, L:BigInt):Boolean = n >= L && isPal(n.toString)
  def isPal(s:String):Boolean = s.reverse == s

  def main(args: Array[String]): Unit = {
    val l = "398904669"
    val r = "13479046850"
     println(superpalindromesInRange("1", "1000000000000000000"))
    println(999 * 999, 1000 * 1000)
    println(BigInt("13479046850") < BigInt("40000800004"))

  }

}
