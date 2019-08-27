/**
  * Maximum Nesting Depth of Two Valid Parentheses Strings
  */
object No1111 {
  object Solution {
    // (())
    // l l r
    // l 1
    // 1 + max()

    // 5 4 3
    // f(5) + f(4) + f(3)
    // (x y z) some(x,y,z) = 4
    case class Elem(depth:Int, idx:Int, isLeft:Boolean)
    def maxDepthAfterSplit(seq: String): Array[Int] = {
      val ans = Array.fill(seq.length)(0)
      // 5001 stand for left brace
      def solve(seq:List[(Char, Int)], stack:List[Elem] = Nil):Unit = seq match {
        case Nil => {}
        case ('(', idx)::t => solve(t, Elem(0, idx, true)::stack)
        case (')', idx)::t =>
          curDepth(stack) % 2 match {
            case 0 => {}
            case 1 =>
              ans(idx) = 1
              stack.dropWhile{case Elem(x, y, bool) => !bool}.head match {
                case Elem(_, idx2, _) => ans(idx2) = 1
              }
          }
          solve(t, f(stack))
      }

      def curDepth(l:List[Elem], acc:Int = 0):Int = l match {
        case Elem(depth, _, false)::t =>curDepth(t, acc max depth)
        case Elem(_,_,true)::t => 1 + acc
      }

      def f(l:List[Elem]):List[Elem] = {
        Elem(curDepth(l), -1, false)::(l.dropWhile{case Elem(d, idx, bool) => !bool}.tail)
      }

      solve(seq.zipWithIndex.toList, Nil)
      //println(ans.toList)
      ans

    }
    // "(()()()()())"
    // [1,0,1,1,1,1,0,1,1,1,1,0,1,1,0,1]
    // ""


  }
}
