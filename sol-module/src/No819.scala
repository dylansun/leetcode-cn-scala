/**
  * Created by lilisun on 3/9/19.
  */
object No819 {

  def mostCommonWord(paragraph: String, banned: Array[String]): String = {
    paragraph
      .toLowerCase()
      .replaceAll("[!?',;.]", " ")
      .split(" ")
      .filter(_ != "")
      .filter(x => !banned.contains(x))
      .groupBy( x => x)
      .map(x => (x._1, x._2.length))
      .toList
      .sortBy(x => (-x._2 , x._1))
      .head._1

  }
  def main(args: Array[String]): Unit = {
    val s = "Bob hit a ball, the hit BALL flew far after it was hit."
    val banned = Array("hit")
    println(s.replaceAll("[!?',;.]", " ")
        .toLowerCase()
      .split(" ")
      .filter(_ != "")
      .filter(x => !banned.contains(x))
        .groupBy(x => x)
      .map(x => (x._1, x._2.length))
        .toList
        .sortBy(x => (-x._2 , x._1))
        .head._1

    )

  }
}
