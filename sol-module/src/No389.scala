
object No389 {
  def findTheDifference(s: String, t: String): Char = {
    val m = s.foldLeft(Map.empty[Char, Int]) { (map, char) => map.updated(char, map.getOrElse(char, 0) + 1) }
    t.foldLeft(m) { (map, char) => map.updated(char, map.getOrElse(char, 0) - 1) }.find(_._2 != 0).get._1
  }
}
