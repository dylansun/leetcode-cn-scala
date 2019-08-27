/**
  * Created by lilisun on 7/1/19.
  */
object No1061 {
  type T = TreeNode
  def maxLevelSum(root: TreeNode): Int = {
    def getSum(l:List[T]):Int = l.map(_.value) sum
    def next(l:List[T]):List[T] = l.flatMap (x => List(x.left, x.right).filter(_!=null))
    def f(l:List[T],cur:Int, max_sum:Int = Int.MinValue, idx:Int =0 ):Int = l match {
      case Nil => idx
      case _ =>
        if(getSum(l) > max_sum) f(next(l), cur+1, getSum(l), cur)
        else f(next(l), cur+1, max_sum, idx)
    }

    f(List(root), 1)
  }

  class DSU(N:Int){
    val parent = (0 until N).toArray
    def find(x:Int):Int = if(x != parent(x)) find(parent(x)) else x
    def union(x:Int, y:Int):Unit = {parent(find(x)) = parent(find(y))}
  }
  def smallestEquivalentString(A: String, B: String, S: String): String = {
    val dsu = new DSU(26)
    (A zip B) foreach {case (ch1, ch2) => dsu.union(ch1-'a', ch2-'a')}
    val table = scala.collection.mutable.HashMap[Char,Char]()
    ('a' to 'z').toList
      .groupBy(x => dsu.find(x - 'a'))
      .values foreach {v => v.foreach {ch => table.put(ch,v.min)}}
    S map table
  }

  def main(args: Array[String]): Unit = {
    val A = "parker"
    val B = "morris"
    println(smallestEquivalentString(A,B,"parser"))
  }
}
