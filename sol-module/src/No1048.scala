/**
  * Created by lilisun on 5/19/19.
  */
import scala.language.postfixOps
object No1048 {
    def longestStrChain(words: Array[String]): Int = {
      val map = words groupBy (x => x.length)
      def f2(x:String)(y:String):Boolean = {
        if(x == "") true
        else if(x.head == y.head) f2(x.tail)(y.tail)
        else x == y.tail
      }
      def f(x:Int):Int = {
        var tmp = map.getOrElse(x, Array.empty[String])
        var idx = x
        var ans = 0
        while(tmp.nonEmpty){
          ans += 1
          idx += 1
          tmp = {
            for {
              y <- map.getOrElse(idx, Array.empty[String])
              if tmp exists(x => f2(x)(y))
            } yield y}
        }
        ans
      }
      {for{ i <- 1 to 16} yield f(i)} max
    }

  def main(args: Array[String]): Unit = {
    val words = Array("a","b","ba","bca","bda","bdca")
    println(longestStrChain(words))
  }
}
