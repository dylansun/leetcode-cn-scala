/**
  * Created by lilisun on 2/26/19.
  */
object No917 {

  def reverseOnlyLetters(S: String): String = {
    var h = 0; var t = S.length - 1; var l =""; var r = ""
    while(h < t){
      (isc(S(h)), isc(S(t))) match {
        case (true, true) => {
          l = l + S(t)
          r = S(h) + r
          h+=1
          t-=1
        }
        case (true, false) => {
          r = S(t) + r
          t -= 1
        }
        case (false, true) => {
          l = l + S(h)
          h += 1
        }
        case (false, false) => {
          l = l + S(h)
          r = S(t) + r
          h+=1
          t-=1
        }
      }
    }
    if(h == t) l + S(h) + r else l + r
  }

  def isc(c: Char):Boolean = c match {
    case x: Char if (x>= 'a' && x <= 'z') || (x>= 'A' && x <= 'Z') => true
    case _ => false
  }
  def main(args: Array[String]): Unit = {


    val s = "sssjjjj-----bbbbbb"
    println(reverseOnlyLetters(s))
    val s2 = "Test1ng-Leet=code-Q!"
    println(reverseOnlyLetters(s2))
  }
}
