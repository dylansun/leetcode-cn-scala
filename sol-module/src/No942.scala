/**
  * Created by lilisun on 3/9/19.
  */
object No942 {
  def diStringMatch(S: String): Array[Int] = {
    var left = 0
    var right = S.length
    var A = List[Int]()
    S.foreach(_ match {
      case 'I' => {A ::= left;left += 1}
      case 'D' => {A ::=right;right -= 1}})
    (right::A).reverse.toArray
  }
}
