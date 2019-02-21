import com.sun.prism.impl.Disposer.Target

/**
  * Created by lilisun on 2/21/19.
  */
object No936 {
  def movesToStamp(stamp: String, target: String): Array[Int] = {
    val fchar = 'X'; var s = target
    val f = target.map(x => fchar)
    val rep = stamp.map(x => fchar)
    var n = 0; var ans = Array[Int]()
    val loopmax = s.length - stamp.length
    while(true){
      var found = false
      for(x <- 0 to loopmax){
        val sub = s.substring(x, x+stamp.length)
        if(isstamp(stamp, sub, fchar)){
          found = true
          println(s"target: $s, sub: $sub, replacement: $rep")
          s = s.substring(0,x) + rep + s.substring(x+stamp.length)
          ans :+= x
          println(s"replaced target: $s, ans: ${ans.mkString}")
        }
      }
      n += 1
      if(n == 10 * target.length || !found){
        if(s == f) return ans.reverse
        else return Array[Int]()
      }
    }
    Array[Int]()
  }
  def isstamp(stamp: String, cdt: String, fchar: Char): Boolean = {
    if(cdt == stamp.map(x => fchar)) return false
    for(x <- stamp.indices if cdt(x) != stamp(x) && cdt(x) != fchar) return false
    true
  }

  def main(args: Array[String]): Unit = {
    val target = "abcdef"
    val na = target.map(x => '?')
    println(na)
    println(na.substring(0,1))


    println(movesToStamp("abca", "aabcaca").mkString)
  }

}
