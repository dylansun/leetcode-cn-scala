/**
  * Created by lilisun on 2/25/19.
  */
object No438 {
  def findAnagrams(s: String, p: String): List[Int] = solver(s,p.sorted, 0, List[Int]())
  def solver(s: String, p: String, i: Int, acc: List[Int]): List[Int] = s match {
    case ""  => acc
    case y: String if y.length < p.length => acc
    case y: String if y.substring(0, p.length).sorted == p => solver(s.tail, p, i+1, i::acc)
    case _ => solver(s.tail, p, i+1, acc)
  }
}
