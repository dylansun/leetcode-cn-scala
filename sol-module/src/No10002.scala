/**
  * Created by lilisun on 4/21/19.
  */
object No10002 {
  class StreamChecker(_words: Array[String]) {
    class Trie(){
      val next = Array.ofDim[Trie](26)
      var endsHere = false
      def insert(s:String):Unit = {
        var node:Trie = this
        for(ch <- s){
          if(node.next(ch - 'a') == null) node.next(ch-'a') = new Trie()
          node = node.next(ch - 'a')
        }
        node.endsHere = true
      }
      def find(s:String):Boolean = {
        var node:Trie = this
        for(ch <- s){
          if(node.endsHere) return true
          if(node.next(ch - 'a') == null) return false
          node = node.next(ch-'a')
        }
        if(node.endsHere) return true
        false
      }
    }
    val words = _words map (x => x.reverse)
    var mem =  List[Char]()
    val max_len = words map (x =>x.length) max
    val trie = new Trie
    words.foreach(word => trie.insert(word))

    def query(letter: Char): Boolean = {
      mem = (letter :: mem ) slice ( 0, max_len)
      trie.find(mem.mkString)
    }

  }

  def twoCitySchedCost(costs: Array[Array[Int]]): Int = {
    val n = costs.length / 2
    val c = costs.map(x => x(1) - x(0)).sorted.slice(0, n)
    costs.map(x=> x(0)).sum + c.sum
  }
  def allCellsDistOrder(R: Int, C: Int, r0: Int, c0: Int): Array[Array[Int]] = {
    val ans = Array.fill(R,C)(0)
    val t = for{
      i <- 0 until R
      j <- 0 until C
    } yield{ (Math.abs(i-r0) + Math.abs(j-c0), Array(i,j))}
    t.sortBy(x => x._1).map(_._2).toArray
  }

  def maxSumTwoNoOverlap(A: Array[Int], L: Int, M: Int): Int = {
    var ans = 0
    for{
      i <- 0 until A.length
      j = i+L
      if j <= A.length
    }{
      val a = A.slice(i,j).sum
      val b = g(A.slice(0,i), M) max g(A.slice(j, A.length), M)
      if(a + b > ans)
        ans = a+b
    }
    ans
  }
  def g(A:Array[Int], k:Int):Int = {
    if (k > A.length) 0 else{
      (for{
        i <- A.indices
        j = i+ k
        if j <= A.length
      } yield A.slice(i,j).sum) max
    }
  }


  def main(args: Array[String]): Unit = {
    val x = allCellsDistOrder(1,100,0,35)
    x.foreach(y => println( y.toList))
  }
}
