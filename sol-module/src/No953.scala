/**
  * Created by lilisun on 3/9/19.
  */
import scala.collection.mutable
object No953 {
  def isAlienSorted(words: Array[String], order: String): Boolean = {

    val mp = mutable.HashMap[Char, Char]()

    (order zip ('a' to 'z').toList)
      .foreach(x => mp(x._1) = x._2)

    (words.map(_.map(mp)) zip words)
      .sortBy(_._1)
      .map(_._2) == words

  }

  def main(args: Array[String]): Unit = {
    val words = List("word","world","row")
    val order =  "worldabcefghijkmnpqstuvxyz"


  }
}
