object ByteDance_byte {
  def lengthOfLongestSubstring(s: String): Int = (for(i <- 0 to s.length) yield longestSubString(s, i)).max


  def longestSubString(s: String, i: Int): Int = {
    val s0 = s.substring(i, s.length)
    var set0 = scala.collection.mutable.Set[Char]()
    for(i <- 0 to s0.length -1 )
      if(!set0.contains(s0.charAt(i))) set0 += s0.charAt(i) else return set0.size

    return set0.size

  }

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

  def checkInclusion(s1: String, s2: String): Boolean = {
    if(s1.length > s2.length) false else {
      val s11 = s1.sorted
      for(i<-0 to s2.length - s1.length){
        if(s2.substring(i, i + s1.length).sorted == s11) return true
      }
      false
    }
  }

  def multiply(num1: String, num2: String): String = {
    var ret = "0"
    val s2 = num2.reverse.toCharArray
    var tail = ""
    for(i <- 0 to s2.size -1 ){
      if(i > 0) tail = tail + "0"
      val t0 = multiplyByChar(num1, s2.charAt(i))
      if(t0 != "0"){
        val t1 = t0 + tail
        ret = add(ret, t1)
      }

    }

    ret
  }

  def multiplyByChar(num1: String, num2: Char): String = {
      if( num2 == '0') return "0"
      val s1 = num1.reverse.toCharArray
      var carry = 0
      var ret = ""
      for(i <- 0 to s1.size - 1){
        val t = (s1(i) - '0')*(num2 - '0') + carry
        carry = t / 10
        ret = ret + ( t % 10  + '0').toChar
      }

      if(carry > 0 ) ret = ret + (carry + '0').toChar

      ret.reverse
  }

  def add(num1: String, num2: String): String = {
    val s1 = num1.reverse.toCharArray
    val s2 = num2.reverse.toCharArray
    var carry = 0
    var ret = ""

    val n = s1.size max s2.size

    for(i <- 0 to n-1){
      val t1 = if(i < s1.size) s1(i) - '0' else 0
      val t2 = if(i < s2.size) s2(i) - '0' else 0

      ret = ret + ((t1 + t2 + carry) % 10 +'0').toChar
      carry = (t1 + t2 + carry) /10
    }

    if(carry == 1) ret = ret + "1"

    ret.reverse
  }


  def restoreIpAddresses(s: String): List[String] = {

    null
  }


  def main(args: Array[String]): Unit = {
    val s1 = "25525511135"

    }
}
