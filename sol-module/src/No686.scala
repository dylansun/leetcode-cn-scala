/**
  * Created by lilisun on 3/11/19.
  */
object No686 {
  def repeatedStringMatch(A: String, B: String): Int = {
    var s = ""
    var r = 0
    while(s.length < B.length){
      s += A
      r +=1
    }
    if(s.contains(B)) return r
    s+=A
    if(s.contains(B)) r+1 else -1

  }
}
