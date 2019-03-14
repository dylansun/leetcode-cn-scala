/**
  * Created by lilisun on 3/14/19.
  */
object No955 {
  def minDeletionSize(A:Array[String]):Int = minDeletionSize(A, Array.fill(A.length)(""), 0)

  def minDeletionSize(A: Array[String],prefix: Array[String], acc: Int): Int = {
    A(0).length match {
      case 0 => acc
      case _ => {
        val h = (prefix zip  A.map(_.head)).map(x => x._1 + x._2)
        h.sorted.toList == h.toList match {

          case false => minDeletionSize(A.map(_.tail), prefix,  acc + 1)
          case true => {
            h.distinct.length == h.length match {
              case  true => acc
              case false => minDeletionSize(A.map(_.tail), h, acc)
            }
          }
        }
      }
    }
  }



  def main(args: Array[String]): Unit = {
    val a = Array("ca","bb","ac")
    val b = Array("xd","xc","yb","za")
    val c = Array("bwwdyeyfhc","bchpphbtkh","hmpudwfkpw","lqeoyqkqwe","riobghmpaa","stbheblgao","snlaewujlc","tqlzolljas","twdkexzvfx","wacnnhjdis")

    println(minDeletionSize(c))

  }
}
