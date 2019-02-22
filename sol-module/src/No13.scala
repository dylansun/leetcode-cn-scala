/**
  * Created by lilisun on 2/23/19.
  */
object No13 {
  val m = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)

  def romanToInt(s: String): Int = (s zip s.tail).foldLeft(m(s.last))( (sum, p) => if(m(p._1) < m(p._2)) sum - m(p._1) else sum + m(p._1))

  def main(args: Array[String]): Unit = {
    val s = "MCMXCIV"
    println(romanToInt(s) == 1994)
  }
}
