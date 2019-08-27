/**
  * Created by lilisun on 4/14/19.
  */
object Contest10001 {

  /**
    * 除数博弈
    */

  val mem = scala.collection.mutable.HashMap[Int, Boolean]()
  def divisorGame(N: Int): Boolean ={
    if(mem.contains(N)) mem(N) else {
      val ans = N match {
        case 1 => false
        case 2 => true
        case 3 => false
        case _ => (1 to N-1) filter (x => N % x == 0) exists (x => !divisorGame(N-x))
      }
      mem.put(N, ans)
      mem(N)
    }
  }

  /**
    * 节点与其祖先之间的最大差值
    */

  def maxAncestorDiff(root: TreeNode): Int = {
    if(root == null) return 0
    var m = 0
    def dfs(r:TreeNode, value:Int):Unit = {
      if(r == null || (r.left == null && r.right == null)){}
      else{
        var queue = List(r.left, r.right) filter (_!= null)
        while(queue.nonEmpty){
          m = m max queue.map(x => Math.abs(value - x.value)).max
          queue = queue flatMap ( x => List(x.left, x.right) filter (_!=null) )
        }
      }
    }
    def traverse(level:List[TreeNode], acc:List[TreeNode]):List[TreeNode] = level match {
      case Nil => acc
      case _ => {
        val nlevel = level flatMap ( x => List(x.left, x.right) filter (_!=null))
        traverse(nlevel, level ++ acc)
      }
    }
    for(node <- traverse(List(root), Nil))
      dfs(node, node.value)
    m
  }
  /**
    * 最长等差数列, scala version TLE, 换成java就过了
    */
  /*
      public int longestArithSeqLength(int[] A) {
        if(A.length < 3) return A.length;
        int ans = 2;
        for(int i = 0 ; i < A.length ; i ++){
            for(int j = i + 1; j < A.length ; j ++){
                int d = A[j]-A[i];
                int count = 2;
                int pre = A[j];
                for(int k = j + 1; k < A.length; k ++){
                    //System.out.println(A[k]+" "+ count +" "+ans);
                    if(A[k] == pre + d){
                        pre += d;
                        count ++;
                    }
                }
                if(count > ans){
                    ans = count;
                }
            }
        }
        return ans;
    }
   */

  /**
    * 从先序遍历还原二叉树
    */
  case class Node(value:Int, depth:Int)
  def recoverFromPreorder(S: String): TreeNode = {

    val nodes = preProcess(S)
    // println(nodes)
    def f(nodes:List[Node], depth:Int = 0):TreeNode = {
      println(nodes, depth)
      nodes match {
        case Nil => null
        case h::t => {
          val root = new TreeNode(h.value)
          t match {
            case Nil => root
            case h1::t1 =>
              var l = t1
              var l1 = List(h1)
              while(l.nonEmpty && l.head.depth > depth + 1){
                l1 ::= l.head
                l = l.tail
              }
              root.left = f(l1.reverse, depth + 1)
              root.right = f(l, depth + 1)
              root
          }
        }
      }
    }
    f(nodes)
  }
  def preProcess(S:String):List[Node] = preProcess(S.toList)
  def preProcess(list:List[Char], acc:List[Node] = Nil):List[Node] = list match{
    case Nil => acc.reverse
    case _ => {
      var ltmp = list
      var count = 0
      while (ltmp.head == '-'){
        ltmp = ltmp.tail
        count += 1
      }
      var num = 0
      while(ltmp != Nil &&ltmp.head != '-'){
        num = num * 10 + (ltmp.head - '0')
        ltmp = ltmp.tail
      }
      preProcess(ltmp, Node(num, count)::acc)
    }
  }
}
