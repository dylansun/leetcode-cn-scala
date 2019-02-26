/**
  * Created by lilisun on 2/27/19.
  */
object No914 {
  def hasGroupsSizeX(deck: Array[Int]): Boolean = 2 <= deck.zipWithIndex.groupBy(_._1).map(_._2.length).reduce(gcd)
  def gcd(x:Int,y:Int):Int= if(x==0) y else gcd(y%x,x)
  def main(args: Array[String]): Unit = {
    println(hasGroupsSizeX(Array(1,1,1,1,3,4,2,2,2,2,2,2)))
  }
}
