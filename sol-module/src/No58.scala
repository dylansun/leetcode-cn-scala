object No58 {
  def lengthOfLastWord(s: String): Int ={
    val words = s.split(" ")
    if(words == null || words.length == 0) return 0
    words.last.length
  }

  def main(args: Array[String]): Unit = {
    val s = "hello world  "
    println(lengthOfLastWord(s))

  }
}
