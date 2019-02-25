/**
  * Created by lilisun on 2/25/19.
  */
object No205 {
  def isIsomorphic(s: String, t: String): Boolean = s match {
    case "" => true
    case _ => {
      val mp = (s zip t).distinct
      mp.map(_._1).length == mp.map(_._1).distinct.length && mp.map(_._2).length == mp.map(_._2).distinct.length
    }
  }
  def main(args: Array[String]): Unit = {
    val s = "abc" zip "ddf"
    println(s)
  }
}
