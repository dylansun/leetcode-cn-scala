/**
  * Created by lilisun on 3/10/19.
  */
object No744 {
  def nextGreatestLetter(letters: Array[Char], target: Char): Char = {
    letters
      .filter(_ > target)
      .sorted
      .headOption
      .getOrElse(letters.min)
  }
}
