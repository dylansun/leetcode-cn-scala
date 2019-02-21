/**
  * Created by lilisun on 2/22/19.
  */
class No392 {
  def isSubsequence(s: String, t: String): Boolean = {
    if(s.length == 0) return true
    if(s.length > t.length) return false
    var si = s.indices.head
    for(x <- t.indices if t(x) == s(si)){
      si += 1
      if(si > s.indices.last) return true
    }
    false
  }
}
