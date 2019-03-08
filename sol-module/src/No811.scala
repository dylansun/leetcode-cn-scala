/**
  * Created by lilisun on 3/8/19.
  */
object No811 {
  def subdomainVisits(cpdomains: Array[String]): List[String] = {
    val ans = scala.collection.mutable.HashMap[String, Int]()
    cpdomains.foreach( x => {
      val t = x.split(" ")
      val c = t(0).toInt
      var domain = t(1)
      ans.put(domain, ans.getOrElse(domain, 0) + c)
      while(domain.indexOf('.') != -1){
        domain = domain.replaceFirst("[a-zA-Z]*.", "")
        ans.put(domain, ans.getOrElse(domain, 0) + c)
      }
    })

    var res = List[String]()
    ans.foreach( x => res ::= (x._2+" "+x._1) )
    res
  }

  def main(args: Array[String]): Unit = {
    var s = Array("900 google.mail.com", "50 yahoo.com", "1 intel.mail.com", "5 wiki.org")
    println(subdomainVisits(s))
  }
}
