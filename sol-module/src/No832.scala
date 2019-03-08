/**
  * Created by lilisun on 3/9/19.
  */
object No832 {
  def flipAndInvertImage(A: Array[Array[Int]]): Array[Array[Int]] = {
    A
      .map(_.reverse)
      .map(_.map(x => (x+1)%2))
  }

  def main(args: Array[String]): Unit = {
    val A = Array(Array(0,1,1))
    val B = A.map(_.reverse).map(_.map(_+1))
    (flipAndInvertImage(A)).foreach(x => println(x.toList))
  }
}
