/**
  * Created by lilisun on 3/10/19.
  */
object No1006 {
  def clumsy(N: Int): Int = {
    N match {
      case 1 => 1
      case 2 => 2
      case 3 => 6
      case _ => clumsy(N-4, N*(N-1)/(N-2)+(N-3))
    }
  }

  def clumsy(N: Int,  acc: Int): Int = {
    if(N < 4){
      N match {
        case 0 => acc
        case 1 => acc - 1
        case 2 => acc - 2*1
        case 3 => acc - 3*2/1
      }
    }
    else{
      clumsy(N-4, acc - N*(N-1)/(N-2)+N-3)
    }

  }

  def main(args: Array[String]): Unit = {
    println(clumsy(4))
    println(clumsy(10))
  }
}
