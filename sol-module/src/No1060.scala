/**
  * Created by lilisun on 8/21/19.
  */
object No1060 {
  object Solution {
    def countCharacters(words: Array[String], chars: String): Int = {
      def toVec(s:String):Array[Int] = {
        val A = Array.fill(26)(0)
        s foreach {ch => A(ch - 'a') += 1}
        A
      }

      def f(A:Array[Int])(s:String):Int = {
        if((A zip toVec(s)) forall {case (x,y) => x >= y}) s.length else 0
      }
      words map f(toVec(chars))  sum
    }
  }
}
