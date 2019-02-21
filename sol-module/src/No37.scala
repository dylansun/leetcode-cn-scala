
object No37 {
  def solveSudoku(board: Array[Array[Char]]): Unit = {
    solver(board)
  }
  def solver(board: Array[Array[Char]]): Boolean ={
    for(i <- 0 to 8; j <- 0 to 8 if board(i)(j).equals('.')){
      for(t <- '1' to '9' if isValid(board,i,j,t)){
        board(i)(j) = t
        if(solver(board)) return true
        else board(i)(j) = '.'
      }
      return false
    }
    true
  }

  def isValid(board: Array[Array[Char]], i: Int, j : Int, num : Char) : Boolean = {
      isRowValid(board,i,j,num) && isColValid(board,i,j,num) && isSqdValid(board, i, j, num)
  }

  def isSqdValid(board: Array[Array[Char]], row: Int, col : Int, num : Char) : Boolean = {
    !subBoard(board, (x , y) => y >= col / 3 *3 && y < col /3 *3+ 3 && x >= row / 3 * 3 && x < row /3 * 3 +3 ).contains(num)
  }

  def isRowValid(board: Array[Array[Char]], row: Int, col : Int, num : Char) : Boolean = {
    !subBoard(board, (x , y) => x == row).contains(num)
  }
  def isColValid(board: Array[Array[Char]], row: Int, col : Int, num : Char) : Boolean = {
    !subBoard(board, (x , y) => y == col).contains(num)
  }

  def subBoard(board:Array[Array[Char]], p: (Int,Int)=>Boolean) : Array[Char] = {
    (for (i <- 0 to 8; j <- 0 to 8 if p(i, j)) yield board(i)(j)).toArray
  }

}
