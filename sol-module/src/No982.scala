/**
  * Created by lilisun on 2/28/19.
  */
object No982 {
  def countTriplets2(A: Array[Int]): Int = {
    (for(x<- A) yield{for(y<-A) yield{ for(z <- A if (x & y & z) == 0) yield
      (x,y,z)
    }}).flatten.flatten.size
  }

  def countTriplets(A: Array[Int]): Int = {
    val m = A.max
    val table = scala.collection.mutable.HashMap[Int, Int]()
    for(i <- 0 to m){
      for( x <- A){
        if((i & x) == 0){
          table.put(i, table.getOrElse(i, 0)+1)
        }
      }
    }
    (for(x <- A) yield{for(y <- A) yield table.getOrElseUpdate( x & y, 0)}).flatten.toList.sum
  }

  def main(args: Array[String]): Unit = {
    println(Int.MaxValue)
  }
}
