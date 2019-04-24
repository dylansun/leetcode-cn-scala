/**
  * Created by lilisun on 4/21/19.
  */
object testTrie {
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
  def main(args: Array[String]): Unit = {
    val words = Array("dba", "dcaa", "dbeq", "acbd")
    val trie = new Trie()
    words.foreach(word => trie.insert(word))
    println(trie.find("dbaefg"))
    println(trie.find(""))
    println(trie.find("dba"))
    println('a'.isLetter,
    'A'.toInt, '0'.toInt)
  }
}
