/**
  * Created by lilisun on 1/10/19.
  */
object No5 {
  def longestPalindrome(s: String): String = {
    if(s == null || s.length() < 1) return ""
    var start = 0
    var end   = 0
    for (i <- 0 to s.length - 1) {
      val len1 = expandAroundCenter(s, i, i)
      val len2 = expandAroundCenter(s, i, i + 1)
      val len = len1 max len2
      if (len > end - start) {
        start = i - (len - 1) / 2;
        end = i + len / 2;
      }
    }
    return s.substring(start, end + 1)

  }


  def expandAroundCenter(s: String, left: Int, right: Int): Int = {
    var l = left
    var r = right
    while(l >= 0 && r < s.length && s.charAt(l) == s.charAt(r)){
      l = l - 1
      r = r + 1
    }

    return  r - l - 1
  }
}
