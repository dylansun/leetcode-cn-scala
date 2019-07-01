/**
  * Created by lilisun on 6/26/19.
  */
object No758 {
  def boldWords(words: Array[String], S: String): String = {
    val flag = Array.fill(S.length)(false)
    for{
      i <- S.indices
      word <- words
      if i + word.length <= S.length
      if S.slice(i, i + word.length) == word
    }(i until i+word.length) foreach {j => flag(j) = true}

    def help(a:Boolean, b:Boolean):String = (a, b) match {
      case (true, false) => "<b>"
      case (false, true) => "</b>"
      case (_,_) => ""
    }
    def f(str:String, i:Int,preFlag:Boolean, acc:String):String =
    if(i == str.length) acc + help(false, preFlag)
    else f(str, i+1, flag(i), acc + help(flag(i), preFlag) + str(i))
    f(S,0,false,"")
  }

  def main(args: Array[String]): Unit = {
    val words = Array("ab","bc")
    val s = "ddabcdd"
    println(boldWords(words, s))
  }

}
