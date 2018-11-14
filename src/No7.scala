/**
  * Created by lilisun on 11/14/18.
  */
object No7 {
  def reverse(x: Int): Int = {
    if( x == -2147483648) return 0
    if(x < 0 ) return -reverse(-x)

    try{
      bounded(x.toString.reverse.toInt)
    }catch {
      case e: Exception => 0
    }
  }
  def bounded(x: Int): Int = {
    if(x < -2147483648 || x > 2147483647) return 0
    x
  }

  def main(args: Array[String]): Unit = {

    println(reverse(1534236469))
    println((231))
  }
}
