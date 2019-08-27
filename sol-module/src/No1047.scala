/**
  * Created by lilisun on 8/22/19.
  * 1047. 删除字符串中的所有相邻重复项
  * 1047. Remove All Adjacent Duplicates In String
  */
object No1047 {
  object Solution {
    def removeDuplicates(S: String): String = {
      def f(l:List[Char], l2:List[Char]):String = {
        (l, l2) match {
          case (Nil, _) => l2.reverse.mkString
          case (h::t, Nil) => f(t, h::Nil)
          case (h1::t1, h2::t2) => if(h1 == h2) f(t1,t2) else f(t1, h1::l2)
        }
      }
      f(S.toList, Nil)
    }
  }
}
