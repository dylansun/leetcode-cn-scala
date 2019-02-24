/**
  * Created by lilisun on 2/25/19.
  */
object No168 {
  /**
    *   1 -> A
        2 -> B
        3 -> C
        ...
        26 -> Z

        27 -> AA
        28 -> AB
    * @param n
    * @param acc
    * @return
    */
  def convertToTitle(n: Int, acc:String = ""): String = n match {
    case x: Int if x <= 0 => ""
    case x: Int if x <= 26 => ('A' + n -1).toChar + acc
    case x: Int if x % 26 ==0 =>convertToTitle(n / 26 -1, ('A'+(n-1) % 26).toChar+acc)
    case _ => convertToTitle(n / 26, ('A'+(n-1) % 26).toChar+acc)
  }

  def main(args: Array[String]): Unit = {
    (1 to 701).foreach(x => println(convertToTitle(x)))
  }
}
