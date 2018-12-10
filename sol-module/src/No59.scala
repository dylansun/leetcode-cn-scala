object No59 {
 def generateMatrix(n: Int): Array[Array[Int]] = {
   val mat = Array.ofDim[Int](n, n)
   var i = 0
   var j = 0
   var direction = 0

   for(t <- 1 to n*n){
     mat(i)(j) = t
     val new_i = getOffset(direction)(0) + i
     val new_j = getOffset(direction)(1) + j

     if(checkValid(new_i, new_j, n , mat)){
       i = new_i
       j = new_j
     }
     else{
       direction = nextDirection(direction)
       i = getOffset(direction)(0) + i
       j = getOffset(direction)(1) + j
     }
   }
   mat
 }

  def checkValid(i: Int, j: Int ,n: Int,  array: Array[Array[Int]]): Boolean = {
    i >= 0 && i < n && j >=0 && j < n && array(i)(j)==0
  }

  def getOffset(direction: Int): List[Int] = {
    val offset = List(List( 0,  1), //right
                      List( 1,  0), //down
                      List( 0, -1), //left
                      List(-1,  0)) //up
    offset(direction)
  }

  def nextDirection(direction: Int): Int = (direction + 1) % 4

  def main(args: Array[String]): Unit = {
    val a1 = Array.ofDim[Int](10, 10)
    val a2 = generateMatrix(10)

    println(a2(0)(0))

  }
}
