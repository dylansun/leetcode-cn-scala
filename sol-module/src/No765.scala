/**
  * Created by lilisun on 2/18/19.
  */
object No765 {
  def minSwapsCouples(row: Array[Int]): Int = {
    var ans = 0
    row.indices.by(2).foreach(x =>{
      val px = row(x) + {if(row(x) %2 == 0) 1 else -1}
      val ip = row.indexOf(px)
      if(ip - x >1){
        println(s"x $x, px $px")
        val temp = row(x+1)
        row(x+1) = px
        row(ip) = temp
        println(row.mkString)
        ans += 1
      }
    }
    )
    ans
  }

  def main(args: Array[String]): Unit = {
    val t1 = Array(5,4,2,6,3,1,0,7)
    println(minSwapsCouples(t1))


  }
}
