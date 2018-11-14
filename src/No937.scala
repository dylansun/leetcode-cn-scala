/**
  * Created by lilisun on 11/14/18.
  */
object No937 {

  def reorderLogFiles(logs: Array[String]): Array[String] = {
      val log_ch = logs.filter(ischartype(_))
      val log_num = logs.filter(!ischartype(_))
      (log_ch.sortWith(comparelogs).toList :::  log_num.toList).toArray
  }
  def comparelogs(l1: String, l2:String): Boolean = {
      val s1 = l1.split(" ")
      val s2 = l2.split(" ")
      val ss1 = s1.slice(1, s1.length - 1).toString
      val ss2 = s2.slice(1,s2.length -1).toString
      ss1 < ss2
  }
  def ischartype(l: String): Boolean = {
    l.split(" ").flatMap(toInt).isEmpty
  }

  def toInt(in: String): Option[Int] = {
       try {
           Some(Integer.parseInt(in.trim))
         } catch {
          case e: Exception => None
           }
     }

  def main(args: Array[String]): Unit = {
    val t1 = Array("a1 9 2 3 1","g1 act car","zo4 4 7","ab1 off key dog","a8 act zoo")
    val res = Array("g1 act car","a8 act zoo","ab1 off key dog","a1 9 2 3 1","zo4 4 7")

    /**
    val l1 = "g1 act car"
    val l2 = "zo4 4 7"
    val n1 = toInt(l1)
    println(l1.split(" ").flatMap(toInt).mkString)
    println(l2.split(" ").flatMap(toInt).mkString)
    println(ischartype(l1))
    println(ischartype(l2))

    val l3 = l2.split(" ")
    val l4 = l3.flatMap(toInt)

    println(l4.mkString)**/
    println(reorderLogFiles(t1).mkString)

  }
}
