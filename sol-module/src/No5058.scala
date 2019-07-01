/**
  * Created by lilisun on 5/12/19.
  */
import scala.collection.mutable

object No5058 {
  object hashMapSolution{
    def longestDupSubstring(S: String): String = {
      val mem = mutable.Map.empty[String , List[Int]]
      for{i <- S.indices}{
        mem(S(i).toString) = i::mem.getOrElse(S(i).toString, Nil)
      }
      val cand = mem filter {case (k, v) => v.length > 1}
      if(cand.toSeq.isEmpty ) "" else
        f(S,2,cand)
    }

    def f(S:String, k:Int, mem:mutable.Map[String ,List[Int]]):String = {
      val new_mem = mutable.Map.empty[String , List[Int]]
      mem.toSeq.foreach{ case (key ,l) =>
        l.foreach { i =>

          if (i + k <= S.length) {
            val substr = S.substring(i, i + k)
            new_mem(substr) = i::new_mem.getOrElse(substr, Nil)
          }
          else {}
        }}
      val cand = new_mem filter {case (k, v) => v.length > 1}
      if(cand.toSeq.isEmpty) mem.toSeq.map{case (key,v) => key}.head
      else f(S, k+1, cand)
    }
  }

  class Trie(){
    val next = Array.ofDim[Trie](26)
    var size = 0
    def insert(s:String):Unit = {
      var node:Trie = this
      for(ch <- s){
        if(node.next(ch - 'a') == null) node.next(ch-'a') = new Trie()
        node.size += 1
        node = node.next(ch - 'a')
      }
      node.size += 1
    }
    def findMax():String = {
      var ans = ""
      var l = this.next.zipWithIndex zip Array.fill(26)("") map {case ((trie, idx), path) => (trie, path + (idx + 'a').toChar)} filter (_._1 != null)
      def f(node:(Trie, String)):Array[(Trie, String)] = {
        if(node._2.length > ans.length) ans = node._2
        node._1.next.zipWithIndex
          .filter {case (trie, idx) => trie != null && trie.size > 1}
          .map {case (trie, idx) => (trie, node._2 + (idx + 'a').toChar)}
      }
      while(l exists { case (trie, path) => trie != null && trie.size > 1}) l = l flatMap f
      ans
    }
    def print():Unit = {
      val l = List(this)
      def f(l:List[Trie]): Unit ={
        if(l exists(_!= null)) {
          println(l filter (_ != null) map (_.size))
          f(l.filter(_ != null) flatMap (_.next))
        }
      }
      f(l)
    }
  }

  def main(args: Array[String]): Unit = {
    val s = "adadadadadba"
    val trie = new Trie()
    for{i <- s.indices} trie.insert(s.substring(i))
    println(trie.findMax())
  }
}
