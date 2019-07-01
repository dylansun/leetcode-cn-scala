/**
  * Created by lilisun on 6/3/19.
  */
object No1071 {
  object Solution {
    def gcdOfStrings(str1: String, str2: String): String = {
      val n = gcd(str1.length, str2.length)
      for {
        len <- n to 1 by -1
        div = str1.slice(0, len)
        if str1.replaceAll(div, "") == ""
        if str2.replaceAll(div, "") == ""
      } return div
      ""
    }
    def gcd (a:Int, b:Int):Int = {
      if(a % b == 0) b else gcd(b, a % b)
    }
  }
}
