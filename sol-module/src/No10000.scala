/**
  * Created by lilisun on 4/7/19.
  */
object No10000 {
  var i = 0
  def removeOuterParentheses(S: String): String = {
    f(S.toList)(S.toList, Nil,Nil) mkString
  }
  def f(s01:List[Char])(s1:List[Char], stack:List[Char], acc:List[Char]):List[Char] = {
    if(i < 10){println(i,s01, s1, stack); i+=1}
    (s01,s1,stack) match {
      case (Nil, _ ,_) => acc
      case (h::t, Nil, Nil) => f(Nil)(Nil, Nil, acc ++ (t.dropRight(1)))
      case (h01::t01, h1::t1, Nil) if s01.length != s1.length => {
        val m = s01.length - s1.length
        val n = s1.length
        if(m!=0) f(s1)(s1, Nil, acc ++ (s01.slice(1,m-1)))
        else f(Nil)(Nil,Nil, acc ++ (s01 slice(1, s01.length -1)))
      }
      case (_, '('::t, _) => f(s01)(t, '('::stack, acc)
      case (_, ')'::t, _) => f(s01)(t, stack.tail, acc)
    }
  }

  def camelMatch(queries: Array[String], pattern: String): Array[Boolean] = {
    val p = insertP(pattern)
    queries map (x => x matches p)
  }
  def insertP(str:String):String = {
    val p = "[a-z]*"
    var ans = p
    for(i <- str.indices) ans = ans + str(i) + p
    ans
  }

  val mod = 1000000007
  def sumRootToLeaf(root: TreeNode): Int = {
    sumr(root, Nil)
  }
  def sumr(root:TreeNode, path:List[Int]):Int = (root, path) match {
    case (null, Nil) => 0
    case (node, _ ) => (node.left, node.right) match {
      case(null, null) =>cpath(root.value::path)
      case(null, r) => sumr(r, root.value::path)
      case(l, null)=> sumr(l, root.value::path)
      case(l, r) => (sumr(l, root.value::path) + sumr(r, root.value::path)) % mod
    }
  }
  // 1 0 0 => 0 0 1
  def cpath(path:List[Int]):Int = {
    var ans = 0
    for( x <- path.reverse) ans = (ans * 2 + x) % mod
    ans
  }

  def videoStitching(clips: Array[Array[Int]], T: Int): Int = {
    solver(T) (clips.toList, 0,0)
  }
  def solver(T:Int)(clips: List[Array[Int]],  t:Int, cost:Int):Int =
    if (t >= T) cost else clips match {
      case Nil => -1
      case _ =>
        val candi = clips.filter(x => x.array(0) <= t).map(x => x.array(1))
        if(candi.isEmpty) -1 else
          solver(T) (clips.filterNot(x => x.array(0) <= t), candi.max, cost+1)
    }


  def main(args: Array[String]): Unit = {
    println(removeOuterParentheses("(()())(())"))
  }
}
