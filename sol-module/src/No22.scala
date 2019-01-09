/**
  * Created by lilisun on 1/10/19.
  */
object No22 {
  def generateParenthesis(n: Int): List[String] = {
    var ans = List[String]()
    if(n == 0)  ans = "" :: ans
    else{
      for(c <- 0 to n-1){
        for(left <- generateParenthesis(c)){
          for(right <- generateParenthesis(n - 1 - c)){
            ans = ("(" + left + ")" + right)::ans
          }
        }
      }
    }

    return ans
  }
}
