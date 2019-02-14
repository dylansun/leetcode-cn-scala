/**
  * Created by lilisun on 2/14/19.
  */
object No38 {
  def countAndSay(n: Int): String = {
    if(n == 1) return "1"
    else{
      var res = "1"
      for(x <- 2 to n) res = countAndSay(res)
      res
    }
  }

  def countAndSay(pre: String): String = {
    if(pre.length == 0) return ""
    pre.prefixLength(_ == pre.head).toString + pre.head + countAndSay(pre.substring(pre.prefixLength(_ == pre.head)))
  }

  def main(args: Array[String]): Unit = {
    for(x <- 1 to 30) println(countAndSay(x))
  }
}
