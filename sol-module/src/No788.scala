/**
  * Created by lilisun on 3/10/19.
  */
object No788 {
  def rotatedDigits(N: Int): Int = {
    (1 to N).count(isRotatedDigits)
  }

  def isRotatedDigits(n: Int): Boolean = {
    n.toString matches "[0182569]*[2569]+[0182569]*"
  }
  def isRotatedDigits2(n: Int): Boolean = {
    val str = n.toString
    if(str.contains('3') || str.contains('4') || str.contains('7')) return false
    val digitMap = Map('2' -> '5', '5' -> '2', '6'-> '9', '9'->'6')
    str.
      map(x => if(digitMap.contains(x)) digitMap(x) else x) != str
  }

  def main(args: Array[String]): Unit = {
    println(rotatedDigits(857))
    val ans = (1 to 857).filter(isRotatedDigits2).toSet -- (1 to 857).filter(isRotatedDigits).toSet
    ans.foreach(println)
    }

}
