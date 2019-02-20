object No316 {
  def removeDuplicateLetters(s: String): String = greedy(s)

  def greedy(s: String):String = {
    val cs: List[Char] = s.toSet.toList.sorted
    for(x <- cs){
      for(y<- 0 until s.length if s.charAt(y) == x){
        val suffix = s.substring(y)
        if(s.toSet == suffix.toSet){
          return x.toString + greedy(suffix.replaceAll(x.toString,""))
        }
      }
    }
    ""
  }

  def bf(s: String): String = {
    val cs = s.toSet
    val it = cs.mkString.sorted.permutations
    while(it.hasNext){
      val p = it.next()
      if(issubstr(s, p)) return p
    }

    ""
  }

  def issubstr(s: String, sub: String): Boolean = {
    val ss = s.toCharArray
    val subs = sub.toCharArray
    var idx = 0
    for(x <- ss.indices if s(x) == subs(idx)) {
      idx += 1
      if(idx == subs.length) return true
    }
    false
  }

  def main(args: Array[String]): Unit = {
    val s = "dafdaoisjfsajglhasjd"
    val cs = s.toCharArray
    val ans = "adoifglhsj"
    println(greedy(s))
    val ss = "cbacdcbc".sorted.toSet
    println(ss)

  }
}
