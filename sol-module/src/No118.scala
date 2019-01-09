object No118 {

  def generate(numRows: Int): List[List[Int]] = {
    // First base case; if user requests zero rows, they get zero rows.
    if (numRows == 0) return List[List[Int]]()
    if(numRows == 1) return List(List(1))
    if(numRows == 2) return List(List(1), List(1,1))
    var triangle = List[List[Int]]()
    // Second base case; first row is always [1].

    triangle = triangle ::: List(List(1)) ::: List(List(1,1))

    for (rowNum <- 2 to numRows-1) {
      val row = new Array[Int](rowNum+1)
      val prevRow = triangle(rowNum-1)

      // The first row element is always 1.
      row(0) = 1

      // Each triangle element (other than the first and last of each row)
      // is equal to the sum of the elements above-and-to-the-left and
      // above-and-to-the-right.
      for (j <- 1 to rowNum - 1) {
        row(j) = prevRow(j-1) + prevRow(j)
      }

      // The last row element is always 1.
      row(rowNum) = 1

      triangle = triangle ::: List(row.toList)
    }

    return triangle
  }

  def main(args: Array[String]): Unit = {
    println(generate(5))

  }

}
