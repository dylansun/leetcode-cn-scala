/**
  * Created by lilisun on 2/22/19.
  */
object No984 {
  def strWithout3a3b(A: Int, B: Int): String = {
    val m = A max B
    val n = A min B
    val mc = if( A > B) "a" else "b"
    val nc = if( A > B) "b" else "a"
    if(m > 2*n+2) return ""
    if(m - n == 0) return (1 to m).map(x => "ab").mkString
    if(m - n == 1) return (1 to n).map(x => "ab").mkString + mc
    if(m - n == 2) return (1 to n).map(x => "ab").mkString + mc + mc
    (1 to m-n-2).map(x => mc + mc + nc).mkString + (1 to 2*n -m +2).map(x => mc + nc).mkString + mc + mc
  }

  def main(args: Array[String]): Unit = {
    val mc = "a"
    val nc = "b"
    val ans0 = (1 to 10).map(x => "ab").mkString
    println(ans0)
    val ans1 = (1 to 10).map(x => "ab").reduce(_+_) + mc
    println(ans1)

    println(strWithout3a3b(1,2))
  }
}
