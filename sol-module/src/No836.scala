/**
  * Created by lilisun on 3/9/19.
  */
object No836 {
  def aInb(i1: (Int, Int), i2: (Int, Int)): Boolean = {
    (i1._2 > i2._1 && i1._2 < i2._2)|| (i1._1 < i2._2 && i1._1 > i2._1)
  }
  def isOverlap(i1: (Int, Int), i2: (Int, Int)): Boolean =  aInb(i2, i1) || aInb(i1, i2)
  def isRectangleOverlap(rec1: Array[Int], rec2: Array[Int]): Boolean = {
    val px1 = (rec1(0) , rec1(2))
    val px2 = (rec2(0) , rec2(2))
    val py1 = (rec1(1) , rec1(3))
    val py2 = (rec2(1) , rec2(3))
   isOverlap(px1, px2) && isOverlap(py1, py2)

  }

  def test(): Unit = {
    val rec1 = Array(7,8,13,15)
    val rec2 = Array(10,8,12,20)
    println(isRectangleOverlap(rec1, rec2))
  }
  def main(args: Array[String]): Unit = {
    test()
  }
}
