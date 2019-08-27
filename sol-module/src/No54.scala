/**
  * Created by lilisun on 8/21/19.
  */
object No54 {
  object Solution {
    def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
      if(matrix.length == 0) return Nil
      val r = matrix.length
      var c = matrix(0).length
      val visited = Array.fill(r, c)(false)
      val dir = Array((0,1),(1,0), (0,-1), (-1, 0))
      def inBound(x:(Int,Int)):Boolean = {
        x._1 >= 0 &&
          x._2 >= 0 &&
          x._1 < r &&
          x._2 < c
      }
      var count = 1
      val need = r * c
      visited(0)(0) = true
      var d = 0
      var pos = (0,0)
      var ans = List(matrix(0)(0))
      while(count < need){
        val npos = (pos._1+ dir(d)._1, pos._2 + dir(d)._2)
        if(inBound(npos) && !visited(npos._1)(npos._2)){
          pos = npos
          count += 1
          visited(npos._1)(npos._2) = true
          ans = ans :+ matrix(npos._1)(npos._2)
        }else{
          d = (d + 1) % 4
        }
      }
      ans
    }
  }
}
