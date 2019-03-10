/**
  * Created by lilisun on 3/10/19.
  */
object No784 {
  def letterCasePermutation(S: String): List[String] = {
    letterCasePermutation(S, 0, List(S))
  }
  def  letterCasePermutation(S: String, i: Int, list: List[String]): List[String] =  {
    if(i == S.length) return list
    S(i) match {
      case ch: Char if ('a' to 'z').contains(ch) => {
        var l = List[String]()
        for(x <- list){
          l ::= x.substring(0,i) + x(i).toUpper + x.substring(i+1, x.length)
          l ::= x
        }
        letterCasePermutation(S, i+1, l)
      }
      case ch: Char if ('A' to 'Z').contains(ch) => {
        var l = List[String]()
        for(x <- list){
          l ::= x.substring(0,i) + x(i).toLower + x.substring(i+1, x.length)
          l ::= x
        }
        letterCasePermutation(S, i+1, l)
      }
      case _ =>  letterCasePermutation(S, i+1, list)
    }
  }
}
