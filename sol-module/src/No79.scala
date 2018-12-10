object No79 {

  def exist(board: Array[Array[Char]], word: String): Boolean = {
    val characters = word.toCharArray
    var start_i = 0
    var start_j = 0
    val row = board.length
    val col = board(0).length
    for(start_i <- 0 to row - 1){
      for(start_j <- 0 to col - 1){
        println(start_i,"  ", start_j , "  ",board(start_i)(start_j), "=====??",characters(0))
        if(board(start_i)(start_j) == characters(0)){
          println(start_i, '*', start_j)
          if(checker(start_i, start_j, board, characters)) return true
        }
      }
    }

    false
  }

  def checker(i: Int, j:Int, board: Array[Array[Char]], char: Array[Char]): Boolean ={

    println(i, "  " ,j)
    if(char.length == 0) return true

    val row = board.length
    val col = board(0).length

    val tmp = board(i)(j)
    board(i)(j) = '*'

    val char_new = char.drop(1)

    if(char_new.length == 0) return true


    val up    = if(i-1 >=0       && board(i-1)(j) == char_new(0)) checker(i-1, j, board, char_new) else false
    if(up) return true

    val down  = if(i+1 <=row - 1 && board(i+1)(j) == char_new(0)) checker(i+1, j, board, char_new) else false
    if(down) return true

    val left  = if(j-1 >=0       && board(i)(j-1) == char_new(0)) checker(i, j-1, board, char_new) else false
    if(left) return true

    val right = if(j+1 <=col - 1 && board(i)(j+1) == char_new(0)) checker(i, j+1, board, char_new) else false

    if(right) return right

    board(i)(j)=tmp
    false

  }

  /**
    * board =
    [
      ['A','B','C','E'],
      ['S','F','C','S'],
      ['A','D','E','E']
    ]

    给定 word = "ABCCED", 返回 true.
    给定 word = "SEE", 返回 true.
    给定 word = "ABCB", 返回 false.
    */
  def main(args: Array[String]): Unit = {
    val board = Array(Array('C','A','A'), Array('A','A','A'), Array('B','C','D'))
    val word = "ABCCED"
    val c = word.toCharArray
    val b = c.drop(2)

   println(exist(board, "AAB"))

    println(board(2)(2))
  }
}
