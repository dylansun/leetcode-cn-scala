/**
  * Alphabet Board Path
  */
object No1138 {
  object Solution {
    def alphabetBoardPath(target: String): String = {
      def f(s:Char, t:Char):String = (pos(s), pos(t)) match {
        case ((x1,y1), (x2, y2)) =>
          var ans = ""
          if(y1 > y2) ans += "L" * (y1 - y2)
          if(y1 < y2) ans += "R" * (y2 - y1)
          if(x1 > x2) ans += "U" * (x1 - x2)
          if(x1 < x2) ans += "D" * (x2 - x1)
          if(s == 'z') ans.reverse + "!" else ans + "!"
      }
      def pos(ch:Char):(Int, Int) = ((ch - 'a') / 5, (ch - 'a') % 5)
      ("a" + target) zip target map {case (s, t) => f(s, t)} reduce (_+_)
    }
  }
}
