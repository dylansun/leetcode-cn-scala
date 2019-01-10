/**
  * Created by lilisun on 1/10/19.
  */
object No537 {
  def complexNumberMultiply(a: String, b: String): String = {
    val (x1, y1) = parseComplexNumber(a)
    val (x2, y2) = parseComplexNumber(b)
    val (x3, y3) = (x1 * x2 - y1 * y2, x1 * y2 + x2 * y1)
    return x3.toString + "+" + y3.toString + "i"
  }

  def parseComplexNumber(a: String): (Int, Int) = {
    val tsp = a.split('+')
    val real = tsp(0).toInt
    val img_part = tsp(1).substring(0, tsp(1).length-1)
    var img = 0
    if(img_part.startsWith("-")) {
      img = - img_part.substring(1, img_part.length).toInt
    }else{
      img = img_part.toInt
    }

    return (real, img)
  }

  def main(args: Array[String]): Unit = {
    val t = "1+-1i"
    val tsp = t.split('+')
    val real = tsp(0).toInt
    val img_part = tsp(1).substring(0, tsp(1).length-1)
    var img = 0
    if(img_part.startsWith("-")) {
      img = - img_part.substring(1, img_part.length).toInt
    }else{
      img = img_part.toInt
    }
    println((real, img))

  }
}
