/**
  * Created by lilisun on 1/10/19.
  */
object No6 {
  def convert(s: String, numRows: Int): String = {
    if (numRows == 1) return s

    val rows = new Array[String](numRows min s.length())
    for(i <- 0 to rows.length -1 ) rows(i) = ""
    var curRow = 0
    var goingDown = false

    for (c <- s.toCharArray()) {
      rows(curRow) = rows(curRow) + c
      if (curRow == 0 || curRow == numRows - 1) {
        goingDown = !goingDown
      }
      curRow = (if (goingDown) 1 else -1) + curRow
    }
    return (rows:\ "")(_+_)
  }

  def main(args: Array[String]): Unit = {
    val s = "PAYPALISHIRING"
    val n = 3
    val res = convert(s, n)
    println( res )


  }
}
