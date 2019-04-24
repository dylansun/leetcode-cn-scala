/**
  * Created by lilisun on 4/21/19.
  */
val words = Array("dba", "dcaa", "dbeq", "acbd")
val trie = new Trie()
words.foreach(word => trie.insert(word))
println(trie.find("dbaefg"))
println(trie.find(""))
println(trie.fin("dba"))

