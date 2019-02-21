import scala.collection.mutable.Stack
/**
  * Created by lilisun on 11/14/18.
  */
object No20 {
  def isValid(s: String): Boolean = {
    val sc = s.toCharArray
    val st = Stack[Char]()

    for(i <- sc.indices){

      if(sc(i) == '(' || sc(i) == '['|| sc(i) == '{'){

        st.push(sc(i))
      }

      if(sc(i) == ')' || sc(i) == ']'|| sc(i) == '}')
      {
        println("right brace")

        if(st.isEmpty || !isPair(st.pop(), sc(i)))
          return false
      }

      println("i ",i, "Stack ", st.mkString)
    }

    println("result ",st.mkString)
    return st.isEmpty
  }

  def isPair(l:Char,r:Char):Boolean = {
    if ((l == '(' && r == ')') ||(l == '[' && r == ']') || (l == '{' && r == '}') ) return true
    else false
  }

  def main(args: Array[String]): Unit = {
    val s1 = "([)]"
    val s2 = "(())(){}{}{{}}"

    println(isValid(s2))
  }
}
