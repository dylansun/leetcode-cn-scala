/**
  * Created by lilisun on 3/15/19.
  */
object No319 {
  def bulbSwitch(n: Int): Int = {
    (1 to n).map(numDivisor).map(_ % 2).sum
  }

  def numDivisor(n: Int): Int = {
    (1 to n).count(x => n % x ==0)
  }

  def main(args: Array[String]): Unit = {
    println(bulbSwitch(3))
  }
}
