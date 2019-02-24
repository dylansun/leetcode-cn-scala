/**
  * Created by lilisun on 2/25/19.
  */
object No171 {
  def titleToNumber(s: String): Int = s.foldLeft(0)((sum, c)=> c - 'A' + 1 + sum * 26)
  def solver(s: String, acc:Int):Int = s match {
    case "" => acc
    case _ => solver(s.tail, s.head - 'A' + 1 + acc * 26)
  }
}
