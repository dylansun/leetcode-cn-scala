import com.sun.prism.impl.Disposer.Target

/**
  * Created by lilisun on 2/21/19.
  */
object No936 {
  def movesToStamp(stamp: String, target: String): Array[Int] = {
    val fchar = 'A'
    val f = target.map(x => fchar)
    var s = target
    val rep = stamp.map(x => fchar)
    val nmax = 10 * target.length; var n = 0
    var ans = Array[Int]()
    val loopmax = s.length - stamp.length
    while(true){
      var found = false
      for(x <- 0 to loopmax){
        val sub = s.substring(x, x+stamp.length)
        //println(s"n: $n, sub: $sub")
        if(isstamp(stamp, sub, fchar)){
          found = true
          println(s"s: $s, sub: $sub, replacement: $rep")
          //s = s.replaceFirst(sub, rep)// here
          s = s.substring(0,x) + rep + s.substring(x+stamp.length)
          ans :+= x
          println(s"replace s: $s, ans: ${ans.mkString}")
        }
      }
      n += 1
      if(n == nmax || !found){
        println(s"found: $found, s: $s")
        if(s == f){
          println(s"ans: ${ans.mkString}")
          return ans.reverse
        }
        else return Array[Int]()
      }
    }
    Array[Int]()
  }
  def isstamp(stamp: String, cdt: String, fchar: Char): Boolean = {
    val qmark = stamp.map(x => fchar)
    if(cdt == qmark) return false
    for(x <- stamp.indices if cdt(x) != stamp(x) && cdt(x) != fchar) return false
    true
  }

  def main(args: Array[String]): Unit = {
    val target = "abcdef"
    val na = target.map(x => '?')
    println(na)
    println(na.substring(0,1))


    println(movesToStamp("rpu", "rprrpurprpuurpu").mkString)
  }

}
