/**
  * Created by lilisun on 6/9/19.
  */
object No_Contest140 {

  def findOcurrences(text: String, a: String, b: String): Array[String] = {
    val words = text.split(' ').toList
    def f(l:List[String], acc:List[String]):List[String] = l match {
      case `a`::`b`::h::t => f(l.tail, h::acc)
      case h::t => f(t, acc)
      case Nil => acc
    }
    f(words, Nil).reverse.toArray
  }


  //
  def smallestSubsequence(text: String): String = {
    val cnt = Array.fill(26)(0)
    text foreach {ch => cnt(ch-'a') += 1}
    def f(l:List[Char], acc:List[Char]):List[Char] = l match {
        case Nil => acc
        case h::t =>
          if(acc.isEmpty) {
            cnt(h - 'a') -= 1
            f(t, h::acc)
          }
          else if(acc.contains(h)) {
              cnt(h - 'a') -= 1
              f(t, acc)
            }
          else if (h < acc.head && cnt(acc.head - 'a') >= 1) f(l, acc.tail)
          else {
              cnt(h- 'a') -= 1
              f(t, h::acc)
            }
          }
    f(text.toList, Nil).reverse.mkString
  }


  object removeSufficientLeaf{
    case class Node(node:TreeNode, kind: String)
    def sufficientSubset(root: TreeNode, limit: Int): TreeNode = {
      def f(l: List[Node]):List[List[Node]] = {
        (l.head.node.left , l.head.node.right) match {
          case (null, null) => List(l)
          case (null, _) => List(Node(l.head.node.right, "r")::l)
          case (_, null) => List(Node(l.head.node.left, "l")::l)
          case (_, _) => List(Node(l.head.node.right, "r")::l, Node(l.head.node.left, "l")::l)
        }
      }

      def solve(paths:List[List[Node]], toleaf:List[List[Node]]):List[List[Node]] = {
        if(paths.nonEmpty){
          val new_paths = paths flatMap f
          solve(new_paths.filter{case h::t => h.node.left != null || h.node.right != null },
            toleaf ++ new_paths.filter{case h::t => h.node.left == null && h.node.right == null }
          )

        }else{
          toleaf ++ paths
        }
      }

      if(root == null ) return null
      if(root.left == null && root.right == null) {
        if(root.value >= limit) return root
        else return null
      }

      val paths = solve(List(Node(root, "Root")::Nil), Nil)
      val unquali = paths.filter{l => l.map(_.node.value).sum < limit}
      if(unquali.isEmpty)  root
      else{
        unquali foreach {case h::t =>
          if(h.kind == "left") t.head.node.left = null
          else if(h.kind == "right") t.head.node.right = null
          else return null
        }
        sufficientSubset(root, limit)
      }
    }
  }

  def main(args: Array[String]): Unit = {
  }
}
