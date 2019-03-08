/**
  * Created by lilisun on 3/9/19.
  */
object No830 {

  def largeGroupPositions(S: String): List[List[Int]] = {
    var left = 0
    var ans = List[List[Int]]()
    for(i <- S.indices){
      S(i) == S(left) match {
        case true => {}
        case false => {
          if(i-1 - left + 1 >= 3) ans ::= List(left, i-1)
          left = i
        }
      }
    }
    if(S.length-1 - left + 1 >= 3) ans ::=List(left,S.length-1 )
    ans
  }
}
