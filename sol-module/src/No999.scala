object No999 {
  def numRookCaptures(board: Array[Array[Char]]): Int = {
    if(board.isEmpty || board(0).isEmpty) return 0
    val offset = Array((1,0), (-1, 0), (0, -1), (0, 1))
    val start = findstart(board)
    var ans = 0
    offset.foreach(x => {
      var pos = (start._1 + x._1, start._2 + x._2)
      while(inbound(pos, board)){
        if(board(pos._1)(pos._2) == '.'){
          pos = (pos._1 + x._1, pos._2 + x._2)
        }
        else if(board(pos._1)(pos._2) == 'B'){
          pos = (-1, -1)
        }
        else{
          ans += 1
          pos = (-1, -1)
        }
      }
    })
    ans
  }

  def inbound(pos: (Int, Int), board: Array[Array[Char]]): Boolean = {
    pos._1 >= 0           && pos._2 >= 0 &&
    pos._1 < board.length && pos._2 < board(0).length
  }

  def findstart(board: Array[Array[Char]]): (Int, Int) = {
    for(x <- board.indices){
      for( y <- board(x).indices){
        if(board(x)(y) == 'R')
          return (x,y)
      }
    }
    (-1,-1)
  }


  def main(args: Array[String]): Unit = {
    val t = Array.ofDim[Char](8,8)
    for(x <- t.indices){
      for( y <- t(0).indices)
        t(x)(y) = '.'
    }
    t(1)(3) = 'p'
    t(0)(3) = 'p'
    t(3)(3) = 'R'
    t(3)(0) = 'p'
    t(3)(1) = 'p'
    t(3)(5) = 'p'
    t(3)(6) = 'B'
    t(5)(3) = 'B'
    t(6)(3) = 'p'
    t.foreach(x => println(x.mkString))
    println(numRookCaptures(t))
  }

}
