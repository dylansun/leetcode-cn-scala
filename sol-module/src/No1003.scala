/**
  * Created by lilisun on 3/5/19.
  */
object No1003 {
  def isValid(S: String): Boolean = {
    if(S == "") true
    else if(S.replace("abc","") == S) false
    else isValid(S.replace("abc",""))
  }

}
