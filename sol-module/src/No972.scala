/**
  * Created by lilisun on 3/13/19.
  */
object No972 {
  def isRationalEqual(S: String, T: String): Boolean = {
    val eps = 1e-8
    Math.abs(parseRational(S) - parseRational(T)) <= eps
  }

  def parseRational(S: String): Double = {
    S.contains("(") match {
      case false => S.toDouble
      case true => {
        val dot = S.indexOf(".")
        val l = S.indexOf("(")
        val r = S.indexOf(")")
        val p = S.split('(').head.toDouble
        val p2 = S.substring(l+1, r).toDouble  / Array.fill(r-l-1)("9").reduce(_+_).toDouble / Math.pow(10, l-dot-1)
        (p + p2)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val S = "8.123(4567)"
    val T = "8.123(4566)"
    val s1 = "7575.7(57)"
    val t1 = "7575.7575(7575)"
    val p = S.split('(').head
    println(parseRational(s1))
    println(parseRational(t1))
    println(isRationalEqual(s1, t1))
    println(66 / 99.0 /1000)
    println(66 / 99.0 /1000  + 0.166) // why the last number is 9 ???
  }
}
