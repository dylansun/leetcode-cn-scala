/**
  * Created by lilisun on 4/24/19.
  */
object No838 {
  def pushDominoes(s: Array[Char]): String = {
    var posR = -1
    val A = for{i <- s.indices}yield s(i) match {
        case '.' => posR
        case 'L' => posR = -1; posR
        case 'R' => posR = i; i
      }

    var posL = -1
    val B = (for{i <- s.indices.reverse} yield s(i) match {
        case '.' => posL
        case 'L' => posL = i; posL
        case 'R' => posL = -1; posL
      }).reverse

    (for{i <- s.indices} yield s(i) match {
        case '.' => (A(i), B(i)) match {
            case (-1,-1) => '.'
            case (-1,_) => 'L'
            case (_, -1) => 'R
            case (a,b) => if(i-a == b - i) '.' else if(i-a < b-i) 'R' else 'L'
          }
        case _ => s(i)
    }).mkString
  }

  def help(A:Array[Char], idx:List[Int]):Unit = {
    var  changed:Int = 0
    var new_idx = List[Int]()
    for{
      i <- idx
      if A(i) == '.'
      l = if(i-1>=0) A(i-1) else '.'
      r = if (i + 1< A.length) A(i+1) else '.'
    } {
     A(i) = (l,r) match {
        case ('R', 'L') => new_idx ::= i;'.'
        case ('R', _) => changed += 1; 'R'
        case (_, 'L') => changed += 1; 'L'
        case _ => new_idx::= i ; '.'
      }
    }
    if(changed != 0) help(A, idx)
  }

  def main(args: Array[String]): Unit = {
  }
}
