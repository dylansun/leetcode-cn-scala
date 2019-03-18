/**
  * Created by lilisun on 3/19/19.
  */
object No950 {
  def deckRevealedIncreasing(deck: Array[Int]): Array[Int] = {
    if(deck.isEmpty) return Array[Int]()
    var output = deck.sortBy(x => -x)
    var ans = Array[Int](output.head)
    output = output.tail
    while(output.nonEmpty){
      ans = Array(output.head) ++ Array(ans.last) ++ ans.dropRight(1)
      output = output.tail
    }
    ans
  }
}
