
object No231 {
  def isPowerOfTwo(n: Int): Boolean =
    if (n <= 0) false
    else (n & (n - 1)) == 0
}
