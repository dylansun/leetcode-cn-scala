import scala.collection.mutable.Stack
object No682 {
  def calPoints(ops: Array[String]): Int = {

    val sk = new Stack[Int]
    for(op <- ops){
      op match {
        case "+" => {
          val top1 = sk.pop()
          val top2 = sk.pop()
          sk.push(top2).push(top1).push(top2 + top1)
        }
        case "C" => sk.pop()
        case "D" => sk.push(sk.top * 2)
        case _ => sk.push(op.toInt)
      }
    }
    sk.sum
  }
}
