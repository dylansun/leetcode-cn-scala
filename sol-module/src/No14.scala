/**
  * Created by lilisun on 11/14/18.
  */
object No14 {
  def longestCommonPrefix(strs: Array[String]): String = {
    if(strs.isEmpty) return ""

    if(strs.length == 1) return strs(0)

    longestCommonPrefix(strs(0), longestCommonPrefix(strs.slice(1, strs.length)))
  }

  def longestCommonPrefix (s1: String, s2: String) : String = {
    if(s1.length == 0 || s2.length ==0 ) return ""
    val n = s1.length min s2.length

    val diff_idx = for(i <- 0 until n if s1.charAt(i) != s2.charAt(i) ) yield i

    //println("dfiff_idx ", diff_idx)
    if(diff_idx.isEmpty)  {
      return s1.substring(0, n)
    }

    s1.substring(0, diff_idx.min )

  }

  def main(args: Array[String]): Unit = {

    val s = Array("a","b")
    println("all ",longestCommonPrefix(s))

    println(s(0).substring(0, 0 ))
    //println("0 1",longestCommonPrefix(s(0), s(1)))
    //println("2 1",longestCommonPrefix(s(2), s(1)))
    //println("2 1 vs 0 1",longestCommonPrefix(longestCommonPrefix(s(2), s(1)), longestCommonPrefix(s(0), s(1))))
    //println(longestCommonPrefix(s(1), s(2)))
  }
}
