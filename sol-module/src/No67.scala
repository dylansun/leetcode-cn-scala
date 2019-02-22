/**
  * Created by lilisun on 2/23/19.
  */
object No67 {
  implicit def charToInt(c: Char): Int = c.toInt

  implicit def intToChar(i: Int): Char = i.toChar

  def addBinary(a: String, b: String): String = {
    @annotation.tailrec
    def addBinary(index1: Int, index2: Int, carry: Int, res: List[Char]): String =
      if (index1 >= 0 && index2 >= 0) {
        val nextCarry = carry + a(index1) - '0' + b(index2) - '0'
        addBinary(index1 - 1, index2 - 1, nextCarry / 2, (nextCarry % 2 + '0') :: res)
      } else if (index1 >= 0) {
        val nextCarry = carry + a(index1) - '0'
        addBinary(index1 - 1, index2 - 1, nextCarry / 2, (nextCarry % 2 + '0') :: res)
      } else if (index2 >= 0) {
        val nextCarry = carry + b(index2) - '0'
        addBinary(index1 - 1, index2 - 1, nextCarry / 2, (nextCarry % 2 + '0') :: res)
      }
      else if (carry == 0) res.mkString("") else res.mkString("1", "", "")

    addBinary(a.length - 1, b.length - 1, 0, Nil)
  }
}
