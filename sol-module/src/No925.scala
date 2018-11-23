object No925 {
  def isLongPressedName(name: String, typed: String): Boolean = {
    var i = 0; var j = 0
    while(i < name.length  || j < typed.length){
      if(j >= typed.length) return false
      val ni = if(i < name.length) name.charAt(i) else ""
      val n0 = if(i == 0) "" else if(i >= name.length) name.last else name.charAt(i-1)
      val tj = typed.charAt(j)

      if(ni == tj){i += 1; j += 1}
      else{
       if(n0 == tj) j+=1
       else return false
      }
    }
    true
  }

  def main(args: Array[String]): Unit = {
    val name = "saeedkkk"
    val typed = "ssaaeedd"
    println(isLongPressedName(name, typed))
  }

}
