/**
  * Created by lilisun on 2/26/19.
  */
object No415 {
  def addStrings(num1: String, num2: String): String = convert(num1.reverse.zipAll(num2.reverse, '0', '0' ).foldLeft((0, ""))((res, x ) => {
    val s = x._1 - '0' + x._2 - '0' + res._1
    (s / 10 , (s % 10).toString + res._2 )
  }))

  def convert(x:(Int, String)): String = if(x._1==1) "1" +x._2 else x._2
}
