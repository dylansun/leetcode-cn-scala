/**
  * Created by lilisun on 5/11/19.
  */
object No770 {
  object recursiveSolution{
    private type Parsed = Map[Seq[String], Int] // Map(Seq("a","a","b") -> 2, Seq() -> 1) is 2*a*a*b +1
    private val zero: Parsed = Map.empty[Seq[String], Int]
    private val one: Parsed = Map(Seq.empty[String] -> 1)

    def basicCalculatorIV(expression: String, evalvars: Array[String], evalints: Array[Int]): List[String] = {
      val vars = evalvars.zip(evalints).toMap

      helper(
        zero,
        expression
          .replace("(", "( ")
          .replace(")", " )")
          .split(" ")
          .filter(_ != "*")
          .map(m => vars.get(m).map(_.toString).getOrElse(m))
      ) // now we need to sort and format the result
        ._1
        .toSeq
        .filter(_._2 != 0)
        .sortWith((a, b) =>
          if (a._1.size != b._1.size)
            a._1.size > b._1.size
          else
            a._1.zip(b._1).dropWhile(p => p._1 == p._2).headOption.exists(h => h._1 < h._2)
        )
        .map { case (s, c) => (c.toString +: s).mkString("*") }(collection.breakOut)
    }
    // parsed - already parsed part of the expression
    // expression - the rest of the expression, which must be parsed
    // mulMode - true if we are in "multiply mode", i.e. when we parse expr like "2*a*b"
    private def helper(parsed: Parsed, expression: Seq[String], mulMode: Boolean = false): (Parsed, Seq[String]) = {
      println("---------------------")
      println(parsed)
      println(expression)
      println(mulMode)
      expression.headOption match {
        case None => (parsed, Seq.empty[String])
        case Some("+" | "-" | ")") if mulMode => (parsed, expression)
        case Some(")") => (parsed, expression.tail)
        case Some("+") =>
          val (rExp, expTail) = helper(one, expression.tail, true)
          helper(sumExp(parsed, rExp), expTail)
        case Some("-") =>
          val (rExp, expTail) = helper(one, expression.tail, true)
          helper(sumExp(parsed, rExp.mapValues(-_)), expTail)
        case _ if !mulMode =>
          val (rExp, expTail) = helper(one, expression, true)
          helper(rExp, expTail)
        case Some("(") =>
          val (rExp, expTail) = helper(zero, expression.tail)
          helper(mulExp(parsed, rExp), expTail, true)
        case Some(v) =>
          val nM = if (v.head.isLetter)
            Map(Seq(v) -> 1)
          else
            Map(Seq.empty[String] -> v.toInt)
          helper(mulExp(parsed, nM), expression.tail, true)
      }
    }

    // sums 2 expressions
    private def sumExp(e1: Parsed, e2: Parsed): Parsed = (e1.keySet ++ e2.keySet)
      .map(k => k -> (e1.getOrElse(k, 0) + e2.getOrElse(k, 0)))
      .filter(_._2 != 0)
      .toMap

    // multiplies 2 expressions
    private def mulExp(e1: Parsed, e2: Parsed): Parsed = e1
      .toSeq
      .flatMap(m1 => e2.map(m2 => (m1._1 ++ m2._1).sorted -> m1._2 * m2._2))
      .groupBy(_._1)
      .map(p => p._1 -> p._2.map(_._2).sum)
      .filter(_._2 != 0)
  }


  def main(args: Array[String]): Unit = {
    val expression = "e * 8 - a + 5"
    val evalvars = Array("e")
    val evalints = Array(1)
    recursiveSolution.basicCalculatorIV(expression, evalvars, evalints) foreach println
  }
}
