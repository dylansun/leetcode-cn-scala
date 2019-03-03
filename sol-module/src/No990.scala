/**
  * Created by lilisun on 3/3/19.
  */
object No990 {
  def equationsPossible(equations: Array[String]): Boolean = {
    val eq = equations.filter(x => x(1) == '=')
    val neq = equations.filter(x => x(1) == '!')
    var e = Set[Set[Char]]()
    for(x <- 'a' to 'z') e =  e + Set(x)
    for(equation <- eq){
      val a = e.filter(_.contains(equation(0)))
      val b = e.filter(_.contains(equation(3)))
      e = e -- a
      e = e -- b
      val c = a.flatten ++ b.flatten
      e = e + c
    }
    for(equation <- neq){
      val a = e.filter(_.contains(equation(0)))
      val b = e.filter(_.contains(equation(3)))
      if(a == b) return false
    }
    true
  }

  def main(args: Array[String]): Unit = {
    var e = Set[Set[Char]]()
    for(x <- 'a' to 'z') e =  e + Set(x)
    println(e.size)

    // a== b
    val a = e.filter( x => x.contains('a'))
    val b = e.filter( x => x.contains('b'))
    e = e -- a
    e = e -- b
    println(e.size)
    val c = a.flatten ++ b.flatten
    println(c.size)
    e = e + c
    println(e.size)
    println(e)

    println("Test")
    val equations = Array("c==c","b==d","x!=z","x==b", "z==c", "c==d")
    println(equationsPossible(equations))


  }
}
