
object No639 {
  def numDecodings(s: String): Int = {
    val len = s.length
    val dp = Array.fill(len)(0L)

    val threshold = scala.math.pow(10, 9).toLong + 7L

    def numDecodingsForIndex(i: Int) = if (i < 0) 1L else dp(i)

    (0 until len).foreach { i =>
      s(i) match {
        case '*' => dp(i) = numDecodingsForIndex(i - 1) * 9
          if (i - 1 >= 0) s(i - 1) match {
            case '1' => dp(i) += numDecodingsForIndex(i - 2) * 9
            case '2' => dp(i) += numDecodingsForIndex(i - 2) * 6
            case '*' => dp(i) += numDecodingsForIndex(i - 2) * 15
            case _ =>
          }
        case '0' => if (i - 1 >= 0) s(i - 1) match {
          case '*' => dp(i) += numDecodingsForIndex(i - 2) * 2
          case '1' => dp(i) = numDecodingsForIndex(i - 2)
          case '2' => dp(i) = numDecodingsForIndex(i - 2)
          case _ =>
        }
        case _ => dp(i) = numDecodingsForIndex(i - 1)  // s(i) could be 1,2,3,4,5,6,7,8,9
          if (i - 1 >= 0) s(i - 1) match {
            case '*' => if (s(i) <= '6') dp(i) += numDecodingsForIndex(i - 2) * 2 else dp(i) += numDecodingsForIndex(i - 2)
            case a if a != '0' => if (s.substring(i - 1, i + 1) <= "26") dp(i) += numDecodingsForIndex(i - 2)
            case _ =>
          }
      }
      if (dp(i) >= threshold)
        dp(i) %= threshold
    }
    dp.lastOption.getOrElse(0L).toInt
  }
}
