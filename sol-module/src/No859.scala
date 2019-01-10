/**
  * Created by lilisun on 1/10/19.
  */
object No859 {

  def buddyStrings(A: String, B: String): Boolean = {
    if (A.length != B.length || A.length == 0) return false;
    if (A.equals(B)) {
      val gp = A.toCharArray.map((_ , 1)).groupBy(_._1)
      (for(g <- gp) yield g._2.length).max > 1
    } else {
      var f1 = -1
      var f2 = -1
      for(i <- 0 to A.length - 1 if A.charAt(i) != B.charAt(i)){
        if(f1 == -1) f1 = i
        else if(f1 != -1 && f2 == -1) f2 = i
        else return false
      }
      return (f2 != -1 && A.charAt(f1) == B.charAt(f2) && A.charAt(f2) == B.charAt(f1))
    }
  }

    def main(args: Array[String]): Unit = {
      val s = "abcdefga"
      val sc = s.toCharArray
      val sg = sc.map((_, 1)).groupBy(_._1)

      println((for(g <- sg) yield g._2.length).max > 1)
    }
}
