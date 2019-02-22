
object No125 {
  def isPalindrome(s: String): Boolean = {
    @annotation.tailrec
    def isPalindrome(lo: Int, hi: Int): Boolean = {
      if (lo >= hi) true
      else if (!s(lo).isLetterOrDigit)
        isPalindrome(lo + 1, hi)
      else if (!s(hi).isLetterOrDigit)
        isPalindrome(lo, hi - 1)
      else if (s(lo).toLower != s(hi).toLower) false
      else isPalindrome(lo + 1, hi - 1)
    }

    isPalindrome(0, s.length - 1)
  }
}
