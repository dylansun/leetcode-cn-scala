/**
  * Created by lilisun on 2/23/19.
  */
object No17 {
  val m = Map('2' -> "abc", '3' -> "def", '4' -> "ghi", '5' -> "jkl", '6' -> "mno", '7' -> "pqrs", '8' -> "tuv", '9' -> "wxyz")

  def letterCombinations(digits: String): List[String] =
    if (digits.isEmpty) Nil else digits.foldLeft(List(""))((ls, digit) => m(digit).flatMap(char => ls.map(_ + char)).toList)
}
