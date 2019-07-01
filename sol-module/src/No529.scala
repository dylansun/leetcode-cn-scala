/**
  * Created by lilisun on 5/9/19.
  */
object No529 {
  def updateBoard(board: Array[Array[Char]], click: Array[Int]): Array[Array[Char]] = {
    val n = board.length
    val m = board(0).length

    def nei(p:(Int, Int)):List[(Int, Int)] = p match {
      case (i,j) => List((i-1,j), (i+1,j),(i-1,j-1),(i-1,j+1), (i,j-1), (i,j+1), (i+1,j-1),(i+1,j+1) )
    }
    def inBound(p:(Int,Int)):Boolean = p match{case (i,j) => i >=0 && j >=0 && i < n && j < m}
    board(click(0))(click(1)) match {
      case 'M' =>
        board(click(0))(click(1)) = 'X'
        board
      case 'E' =>
        def change(p:(Int,Int)):Unit = p match {
          case (i,j) => board(i)(j) match {
            case 'M' => board(i)(j)  = 'X'
            case 'E' => nei(p) filter inBound count {case (i1,j1) => board(i1)(j1)== 'M'} match {
              case 0 => board(i)(j) = 'B'
              case cnt => board(i)(j) = ('0' + cnt).toChar
            }
          }
        }
        def dfs(l:List[(Int,Int)]):Unit = l match {
          case Nil => {}
          case _ =>
            l foreach change
            dfs((l filter {case (i,j) => board(i)(j)== 'B'} flatMap nei filter inBound filter {case(i,j) => board(i)(j) == 'E'}).distinct)

        }
        dfs(List((click(0), click(1))))
        board
      case _ => board
    }
  }

  def test ():Unit = {
    val board = Array(
      Array('B', '1', 'E', '1', 'B'),
      Array('E', 'E', 'M', 'E', 'E'),
      Array('E', 'E', 'E', 'E', 'E'),
      Array('E', 'E', 'E', 'E', 'E')
    )
    board.map(_.toList) foreach println
    println("-------------")
    val ans = updateBoard(board, Array(1,2))
    ans.map(_.toList) foreach println
  }
  def main(args: Array[String]): Unit = {
    test()
  }
}
