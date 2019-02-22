object No91 {
  def numDecodings(s: String): Int = {
    val len = s.length
    val dp = Array.fill(len)(0)

    def numDecodingsForIndex(i: Int) = if (i < 0) 1 else dp(i)

    (0 until len).foreach { i =>
      if (s(i) != '0')
        dp(i) = numDecodingsForIndex(i - 1)
      if (i - 1 >= 0 && s(i - 1) != '0' && s.substring(i - 1, i + 1) <= "26")
        dp(i) += numDecodingsForIndex(i - 2)
    }
    dp.lastOption.getOrElse(0)
  }
}
