/**
  * Created by lilisun on 3/9/19.
  */
object No796 {


  def rotateString(A: String, B: String): Boolean = {
    (A + A).replaceFirst(B, "") == A && A.length == B.length
  }
  def rotateString3(A: String, B: String): Boolean = {
    if(A.length != B.length ) false
    else if( A.length == 0 ) true
    else{
      (0 to A.length)
        .toList
        .map(x => A.substring(x, A.length) + A.substring(0, x))
        .contains(B)
    }
  }
  def rotateString2(A: String, B: String): Boolean = {
    if(A.length != B.length) false
    else{
      val n = A.length
      var rotate = A
      for(_ <- 0 until n){
        if((rotate.tail + rotate.head) == B) return true
        else rotate = rotate.tail + rotate.head
      }
      false
    }
  }
}
