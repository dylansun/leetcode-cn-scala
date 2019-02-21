/**
  * Created by lilisun on 11/14/18.
  */
object No771 {
  def numJewelsInStones(J: String, S: String): Int = {

    def jcount(c: Char) : Int = {
      if(J.toCharArray.contains(c)) return  1
      0
    }

    val k = S.toCharArray
    (for(i <- k.indices) yield jcount(k(i))).sum
  }


}
