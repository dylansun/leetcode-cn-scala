object No929 {
  def numUniqueEmails(emails: Array[String]): Int = emails.map(func(_)).toSet.size

  def func(s: String): String = removeDot(removePlus(s.split("@")(0))) + "@" + s.split("@")(1)

  def removePlus(in:String):String = if(in.indexOf("+") < 0) in else in.substring(0, in.indexOf("+"))

  def removeDot(in: String): String = in.replace(".", "")

  def main(args: Array[String]): Unit = {
    val c1 = Array("test.email+alex@leetcode.com","test.e.mail+bob.cathy@leetcode.com","testemail+david@lee.tcode.com")
    val c2 = c1.flatMap((func(_)))
    println(c2(0))

    println(c2.mkString)

    println(numUniqueEmails(c1))
  }
}
