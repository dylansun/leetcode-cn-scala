/**
  * Created by lilisun on 6/2/19.
  */
object No5078 {

  // 1
  // 1 => carry = -1
  // 1
  // 1
  // carry = 1 => 1 , carry = - 1
  // 0
  // 0
  // -1 => 1 => carry = 1
  def addNegabinary(A: Array[Int], B: Array[Int]): Array[Int] = {
    def addBit(x:Int, y:Int, carry:Int):Int = {
      x + y + carry match {
        case 0 => 0
        case 1 => 1
        case -1 => 1
        case 2 => 0
        case 3 => 1
      }
    }
    def calCarry(x:Int, y:Int, carry:Int):Int = {
      x + y + carry match {
        case 0 => 0
        case 1 => 0
        case -1 => 1
        case 2 => -1
        case 3 => -1
      }
    }
    def f(l1:List[Int], l2:List[Int], carry :Int = 0, acc:List[Int]):List[Int] = {
      (l1, l2) match {
          case (h1::t1, h2::t2) => f(t1, t2, calCarry(h1,h2,carry),addBit(h1, h2, carry)::acc)
          case (h1::t1, Nil) => f(t1,Nil, calCarry(h1, 0, carry), addBit(h1, 0, carry)::acc)
          case (Nil, h1::t1) => f(t1,Nil, calCarry(h1, 0, carry), addBit(h1, 0, carry)::acc)
          case (Nil, Nil) => carry match {
            case 0 => acc
            case 1 => 1::acc
            case -1 => 1::1::acc
          }
      }
    }
    def rmzero(l:List[Int]):List[Int] = l match {
      case 0::h::t => rmzero(h::t)
      case _ => l
    }
    rmzero(f(A.reverse.toList, B.reverse.toList, 0, Nil)).toArray
  }

  def main(args: Array[String]): Unit = {
    println(addNegabinary(Array(1,0,1), Array(1,0,1)).toList)
  }
}
