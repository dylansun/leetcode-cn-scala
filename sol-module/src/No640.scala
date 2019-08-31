/**
  * Created by lilisun on 8/30/19.
  */
object No640 {
  object Solution {
    case class Coeff(n:Int, bias:Int){
      def +(that: Coeff):Coeff = Coeff(n + that.n, bias + that.bias)
      def -(that: Coeff):Coeff = Coeff(n - that.n, bias - that.bias)
      def get():String = this match {
        case Coeff(0, 0) => "Infinite solutions"
        case Coeff(0, _) => "No solution"
        case _ => "x=" + (-bias / n)
      }
    }

    def notOp(ch:Char):Boolean = ch != '+' && ch != '-'
    def parser(str:String, acc:List[Coeff] = Nil):List[Coeff] = str match {
      case "" => acc
      case _ => parser(str.tail dropWhile notOp, trans(str.head + str.tail.takeWhile(notOp))::acc)
    }

    def trans(str:String):Coeff = {
      if(str.contains('x')) Coeff(myInt(str.replace("x","")), 0)
      else Coeff(0, str.toInt)
    }
    def myInt(str:String):Int = str match {
      case "" => 1
      case "-" => -1
      case "+" => 1
      case _ => str.toInt
    }
    def solveEquation(equation: String): String = equation split "=" match {
      case Array(str1, str2) => (parser(str1).reduce(_+_) - parser(str2).reduce(_+_)).get()
    }


  }
}
