/**
  * Created by lilisun on 5/3/19.
  */
object No779 {
    def kthGrammar(N: Int, K: Int): Int = f(0, N,K)
    def f(x:Int, y:Int, z:Int):Int = y match {
      case 1 => x % 2
      case _ =>
        if( z > Math.pow(2, y-2)) f(x + 1, y-1, z -  Math.pow(2, y-2).toInt)
        else f(x, y-1, z)
    }

  def main(args: Array[String]): Unit = {
    println(kthGrammar(6,9))
  }
}
