/**
  * Created by lilisun on 3/11/19.
  */
object No696 {

  def countBinarySubstrings2(s: String): Int = {

    var last = 0
    var cur = 1
    var res = 0
    for( i <- 1 until s.length ){
      if(s(i) == s(i-1)) cur +=1
      else{last = cur; cur =1}
      if(last >= cur) res +=1
    }
    res
  }
  def countBinarySubstrings(s: String): Int = {
    def isBS(f: Int, u: Int):Boolean = {
      val sub = s.substring(f, u)
      val sub1 = sub.substring(0, sub.length / 2)
      val sub2 = sub.substring(sub.length / 2, sub.length)
      sub1.distinct.length == 1 && sub2.distinct.length == 1 && sub1.head != sub2.head
    }
    var ans = 0
    for(i <- 0 until s.length){
      for( j <- (i+2 to s.length).by(2) if isBS(i, j)){
        ans += 1
      }
    }

    ans
  }
}
