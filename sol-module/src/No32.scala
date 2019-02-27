/**
  * Created by lilisun on 2/28/19.
  */
object No32 {
  def longestValidParentheses(s: String): Int = solver1(s)
  //TLE solver2 TODO
  def solver2(s: String):Int = {
    var t0 = s.map(x => if(x == '(') 'l' else 'r')
    var t1 = t0.replace("lr", "#")
    if(t0 == t1) return 0
    t0 = t1
    var count = 1
    while(count <= s.length / 2 ){
      val tag = ntag(count, "#", "")
      val patten = "l"+tag+"r"
      t1 = t0
      t0 = t1.replaceAll(patten, "#"+tag)
      count += 1
    }
    (t1.split("l|r") map f).max * 2
  }

  def ntag(x:Int, tag: String, acc: String):String = x match {
    case 0 => acc
    case _ => ntag(x-1,tag,tag + acc)
  }
  def f(x:String):Int = x.length

  def solver1(s: String): Int =
  {
    val candidate = for(x <- 0 until s.length) yield lvp(s.toCharArray.toList.slice(x, s.length), Nil, 0, 0)
    candidate.max
  }

  def lvp(s: List[Char], stack: List[Char], c: Int, m: Int): Int = {
    s match {
      case Nil => m
      case '('::t0 => lvp(t0, '(' :: stack, c, m)
      case ')'::t0 => stack match {
        case Nil => lvp(t0, Nil, 0 , m max c)
        case '('::t1 => if(t1.isEmpty){
          lvp(t0, t1, c+2, m max (c+2))
        }else{
          lvp(t0, t1, c+2, m)
        }
      }
    }
  }
  def main(args: Array[String]): Unit = {
    val s = "()(()(())("
    val ans = "r#l#l##l"

    val k = ans.split("l|r") map f
    println(k.mkString)
    println(longestValidParentheses("(()())"))
  }
}
