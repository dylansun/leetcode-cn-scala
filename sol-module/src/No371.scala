
object No371 {
  @annotation.tailrec
  def getSum(a: Int, b: Int): Int = {
    if (b == 0) a
    else getSum(a ^ b, (a & b) << 1)
  }
}
