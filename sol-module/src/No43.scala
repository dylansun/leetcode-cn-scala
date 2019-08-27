object No43 {
  object Solution {

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
  }
}
