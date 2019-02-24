/**
  * Created by lilisun on 2/25/19.
  */
object No28 {
  def strStr(haystack: String, needle: String): Int = solver(haystack, needle, 0)
  def solver(h: String, n:String, acc: Int): Int = h match {
    case x: String if x.length < n.length => -1
    case x: String if x.startsWith(n) => acc
    case _ => solver(h.tail, n, acc + 1)
  }
}
