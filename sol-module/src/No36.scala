object No36 {
  object Solution {
    def isValidSudoku(board: Array[Array[Char]]): Boolean = {
      board.foreach(x => println(x.mkString))
      var parts = List[List[Char]]()
      // row
      for(i <- 0 until 9) parts ::= (board(i) toList)
      // col
      for(j <- 0 until 9){
        var tmp = List[Char]()
        for(i <- 0 until 9) tmp ::= board(i)(j)
        parts ::= tmp
      }
      //dig
      /*var tmp = List[Char]()
      for(i <- 0 until 9) tmp ::= board(i)(i)
      parts ::= tmp

      tmp = Nil
      for(i <- 0 until 9) tmp ::= board(i)(8 -i)
      parts ::= tmp
      */
      //small region
      val topleft = List((0,0),(3,0),(6,0),(0,3),(0,6),(3,3),(3,6),(6,3),(6,6))
      topleft.foreach{ x =>
        var tmp = List[Char]()
        for{i <- 0 until 3;j <- 0 until 3} tmp ::= board(x._1 + i)(x._2 + j)
        parts ::= tmp
      }
      parts map (x => x filter (_ != '.')) forall valid
    }
    def valid(list:List[Char]):Boolean = {
      //println(list)
      list.length == list.distinct.length
    }
  }
}
