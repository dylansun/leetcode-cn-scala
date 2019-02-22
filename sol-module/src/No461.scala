
object No461 {
  def hammingDistance(x: Int, y: Int): Int = {
    @annotation.tailrec
    def calDistance(x: Int, acc: Int): Int = {
      if (x == 0) acc
      else calDistance(x & (x - 1), 1 + acc)
    }

    calDistance(x ^ y, 0)
  }
}