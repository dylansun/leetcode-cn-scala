/**
  * Created by lilisun on 3/24/19.
  */
object No1023 {
  def queryString(S: String, N: Int): Boolean = {
    val maxlen = 30
    var subs = Set[String]()
    for(i <- 1 to 30){
      for(j <- 0 to  S.length - i){
        subs += S.substring(j, j+i)
      }
    }
    println(subs)
    for(i <- 1 to N if ! subs.contains(num2str(i))) return false
    true

  }

  def num2str(n:Int):String = {
    var ans = ""
    var t = n
    while(t != 0){
      ans = (t % 2).toString + ans
      t = t >> 1
    }
    ans
  }
}
