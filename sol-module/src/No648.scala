/**
  * 648. 单词替换
  */
object No648 {
  object BruteForce {
    def replaceWords(l: List[String], sentence: String): String = {
      val dict = l.sortBy{x => x.length}
      def f(str:String):String = dict dropWhile {x => !str.startsWith(x)} sortBy {x => x.length} match {
        case Nil => str
        case h::t => h
      }

      sentence split (" ") map f mkString(" ")
    }
  }
  object UseTrie{
    class  Trie {
      var endsHere = false
      val next = Array.ofDim[Trie](26)
      def insert(str:String):Unit = {
        var node = this
        for{ ch <- str}{
          if(node.next(ch - 'a') != null) node = node.next(ch - 'a')
          else {
            node.next(ch-'a') = new Trie()
            node = node.next(ch - 'a')
          }
        }
        node.endsHere = true
      }

      def fetch(str:String):String = {
        var node = this
        var l = List.empty[Char]
        for { ch <- str}{
          if(node.endsHere) return l.reverse.mkString
          if(node.next(ch - 'a') == null) return str
          node = node.next(ch - 'a')
          l ::= ch
        }
        str
      }

      def replaceWords(l: List[String], sentence: String): String = {
        val trie = new Trie()
        l foreach trie.insert

        sentence split " " map trie.fetch mkString " "
      }
    }
  }

  def main(args: Array[String]): Unit = {

  }
}
