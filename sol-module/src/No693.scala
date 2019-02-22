
object No693 {
  def hasAlternatingBits(n: Int): Boolean = {
    def hasOnlyOneBit(n: Int): Boolean =
      (n & (n - 1)) == 0

    val t = n ^ (n >> 1)
    hasOnlyOneBit(t ^ (t >> 1))
  }
}
