/**
  * Created by lilisun on 2/15/19.
  */
object No51 {
  def totalNQueens(n: Int): Int = genCands(n).size
  def solveNQueens(n: Int): List[List[String]] = genCands(n)

  def genCands(n: Int): List[List[String]] = {

    var res = List[List[String]]()
    for(x <- (0 until n).permutations){
      if(checkDiagonal(x.toList)){
        val candidate = Array.ofDim[String](n,n)
        for(xx <- 0 until n) for( yy <- 0 until n){
          if(x(xx) == yy) candidate(xx)(yy) = "Q"
          else candidate(xx)(yy) = "."
        }

        //print
        for(x <- 0 until n) {
          print(s"lien $x:")
          for(y <- 0 until n){
            print( s"${candidate(x)(y)}")
          }
          println()
        }

        res = (for(x <- 0 until n) yield candidate(x).reduce(_+_)).toList :: res
        //res = candidate.map((_.reduce(_+_))) :: res
      }

    }

    res
  }

  def checkDiagonal(l: List[Int]): Boolean = {
    //println(s"check dianal for l: $l")
    val n = l.length
    for(x <- 0 until n ){
      for(y <- x+1 until n){
        if(Math.abs((x - y).toDouble/(l(x)-l(y)).toDouble) == 1) {
          println(s"x: $x, y : $y, l(x): ${l(x)}, l(y): ${l(y)}")
          return false
        }
      }
    }
    true
  }

  def main(args: Array[String]): Unit = {
    val x = genCands(5)
    val y = List(4 ,2 ,0 ,3 ,1)
    val b = checkDiagonal(y)
    println(x)
  }
}
