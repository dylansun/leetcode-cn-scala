
/**
  * 4 → 16 → 37 → 58 → 89 → 145 → 42 → 20 ->4
  */
object No202 {

  def isHappy(n: Int): Boolean = {
    val k = squaresum(n)
    if(k == 1) return true
    if(k == 4) return false
    isHappy(k)
  }
  def squaresum(n: Int ): Int = {
    var ret = 0
    var it = n

    while(it / 10 > 0 ){
      ret = ret + (it % 10 )*(it % 10 )
      it = it / 10
    }

    ret = ret + (it % 10 )*(it % 10 )

    return ret
  }

  def main(args: Array[String]): Unit = {
    println(isHappy(37))
  }

}
