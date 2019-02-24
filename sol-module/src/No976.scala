object No976 {
  def largestPerimeter(A: Array[Int]): Int = solver(A.sorted.reverse)
  def solver(A: Array[Int]): Int = {
    if(A.length < 3) 0
    else if( A(0) <  A(1) + A(2)) A(0) + A(1) + A(2)
    else solver(A.tail)
  }
}
