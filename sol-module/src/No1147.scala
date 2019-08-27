object No1147 {
  object Solution {
    def longestDecomposition(A: String): Int = {
      def f(i:Int, j:Int, acc:Int = 0):Int = {
        println(i,j, acc)
        if(i > j) acc else {
          g(i,j) match {
            case n => if(j-n+1 == i) f(i+n, j-n, acc+1) else f(i+n, j-n, acc+2)
          }

        }
      }
      def g(i:Int, j:Int, d:Int = 1 ):Int = {
        if(A.substring(i,i+d) == A.substring(j-d+1,j+1)) d
        else g(i,j, d+1)
      }

      f(0, A.length-1, 0)
    }
  }
}
