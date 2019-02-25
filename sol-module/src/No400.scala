/**
  * Created by lilisun on 2/25/19.
  */
object No400 {
  def findNthDigit(n: Int): Int = solver(n, 1)

  def solver(n: Int, idx: Int): Int = {
    n match {
      case x: Int if n <= 9 * idx * Math.pow(10, idx - 1) => {
        val whichnum = (Math.pow(10, idx - 1).toInt + (n-1) / idx ).toString
        println(s"n: $n, idx: $idx, whichnum : $whichnum, shift: ${(n+1) % idx}")
        whichnum((n - 1) % idx ) - '0'
      }
      // 10 => 1, 1=> 10 shift 0
      // 11 => 1, 2=> shift 1
      case _ => solver(n - 9 * idx * Math.pow(10, idx - 1).toInt, idx + 1)
    }
  }


  def main(args: Array[String]): Unit = {
    println(findNthDigit(1000))
  }
}
