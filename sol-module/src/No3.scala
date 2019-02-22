object No3 {
  def lengthOfLongestSubstring(s: String): Int = (for(i <- 0 to s.length) yield longestSubString(s, i)).max
  def longestSubString(s: String, i: Int): Int = {
    val s0 = s.substring(i, s.length)
    var set0 = scala.collection.mutable.Set[Char]()
    for(i <- 0 until s0.length  )
      if(!set0.contains(s0.charAt(i))) set0 += s0.charAt(i) else return set0.size
    set0.size
  }

  def main(args: Array[String]): Unit = {

  }
}
