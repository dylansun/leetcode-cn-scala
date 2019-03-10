/**
  * Created by lilisun on 3/10/19.
  */
object No754 {
  case class state(sum: Int, nextStep: Int)
  def reachNumber(target: Int): Int = {

    val s = geNumber(Math.abs(target))
    s.sum - target match {
      case 0 => s.nextStep - 1
      case remain: Int => (remain % 2, s.nextStep % 2) match {
        case (1, 0) => s.nextStep + 1
        case (1, 1) => s.nextStep
        case _ => s.nextStep -1
      }
    }
  }

  def geNumber(target: Int):state = {
    var sum = 0
    var nextStep = 1
    while(sum < target){
      sum += nextStep
      nextStep +=1
    }
    state(sum, nextStep)
  }

  def main(args: Array[String]): Unit = {
    println(reachNumber(-2))
  }
}
