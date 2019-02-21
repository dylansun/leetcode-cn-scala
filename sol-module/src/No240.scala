/**
  * Created by lilisun on 2/21/19.
  */
object No240 {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = matrix.length != 0 && matrix(0).length != 0 && matrix.map( x => x.contains(target)).reduce(_||_)

  def bisearch(matrix: Array[Array[Int]], target: Int): Boolean = {
    true
  }

  def move(matrix: Array[Array[Int]], target: Int): Boolean = {
    if(matrix.length == 0 || matrix(0).length == 0) return false
    var i = 0; var j = matrix(0).length -1
    while( i < matrix.length && j >= 0){
      if(matrix(i)(j) == target) return true
      else if(matrix(i)(j) < target) i += 1
      else j -= 1
    }
    false
  }
}
