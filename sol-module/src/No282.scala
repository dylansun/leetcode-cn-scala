/**
  * Created by lilisun on 2/28/19.
  */
object No282 {
  def addOperators(num: String, target: Int): List[String] = {
    val op = List("+","-","*","")
    var candidate = List(List("+"),List("-"), List("*"), List(""))
    var s = num.length-1
    while(s>1){
      var tmp = List[List[String]]()
      for(x <- op)for(y <- candidate) tmp = (x::y)::tmp
      candidate = tmp
      s -= 1
    }
    for(x <- candidate if isgood(merge(num,x)) && cal(merge(num,x)) == target ) yield merge(num,x)

  }

  def merge(s:String, l: List[String]):String = {
    if(s.length - l.length != 1) ""
    else{
      var ans = s.head.toString
      for(x<- 1 until s.length){
        ans = ans + l(x-1) + s(x)
      }
      ans
    }
  }

  def isgood(s:String):Boolean = {
    for(x <- s.split("\\+|-|\\*") if x.startsWith("0") && x != "0" )return false
    true
  }
  def cal(exp: String): Long = exp match {
    case "" => 0
    case x: String if x.contains('+') => exp.split('+').map(x => cal(x)).sum
    case x: String if x.contains('-') =>
      val tmp = exp.split('-')
      cal(tmp.head) - tmp.tail.map(x => cal(x)).sum
    case x: String if x.contains('*') => exp.split('*').map(x => cal(x)).product
    case _ => exp.toLong
  }

  def cal2(exp: String): Long = {
    //println(s"Calculate: $exp")
    if(exp == "") return 0
    if(exp.contains('+')){
      exp.split('+').map(x => cal(x)).sum
    }
    else if(exp.contains('-')){
      val tmp = exp.split('-')
      cal(tmp.head) - tmp.tail.map(x => cal(x)).sum
    }else if(exp.contains('*')){
      exp.split('*').map(x => cal(x)).product
    }else{
      exp.toLong
    }
  }

  def main(args: Array[String]): Unit = {
    //val s = "13*12+11+22*23-41"
    println("".startsWith("0"))
    println(s"result: ${addOperators("105",5)}")
  }
}
