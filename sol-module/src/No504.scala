object No504 {
  def convertToBase7(num: Int): String = if (num == 0) "0" else {
    val res = StringBuilder.newBuilder

    def loop(num: Int): String =
      if (num == 0) res.toString.reverse
      else {
        res.append(num % 7)
        loop(num / 7)
      }

    val negative = if (num < 0) true else false
    if (negative) "-" + loop(num.abs) else loop(num.abs)
  }
}
