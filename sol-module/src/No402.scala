/**
  * Created by lilisun on 2/22/19.
  */
object No402 {
  def removeKdigits(num: String, k: Int): String = {
    if(k >= num.length || num.length == 0) return "0"
    var l = List[Char]()
    var count = 0; var x = num.indices.head
    while(count < k ){
      if(l.isEmpty|| (x <= num.indices.last  && l.head <= num(x))) {
        l ::= num(x)
        x += 1
      }
      else {l = l.tail; count += 1}
      println(s"l: ${l.reverse.mkString}, count: $count, x: $x, k: $k")
    }

    val remain =  if(x <= num.indices.last) num.substring(x) else ""
    removelzero(l.reverse.mkString + remain)
  }

  // WA, only partial minimal
  def removeKdigits_a(num: String, k: Int): String = {
    if(k >= num.length) return "0"
    var rx = List[Int]()
    var vc = Set[Int]()
    (0 to k).foreach(x => vc += x) // add k+1 num
    var p = k + 1
    while(rx.length < k){
      val max = findmax(num, vc)
      rx = max::rx
      vc -= max
      if(p <= num.indices.last){
        vc += p
        p += 1
      }
    }
    removelzero(genres(num, rx))
  }
  def genres(nums: String, s: List[Int]): String = {
    (for(x <- nums.indices if !s.contains(x)) yield nums(x)).mkString
  }
  def findmax(num: String, s: Set[Int]): Int = {
    (for(x <- s) yield (x, num(x) - '0')).reduce( (x, y) => if(x._2 > y._2) x else y)._1
  }

  def removelzero(num: String): String = {
    if(num == "0") "0"
    else if(num.startsWith("0")) removelzero(num.tail)
    else num
  }

  def main(args: Array[String]): Unit = {
    val s = "000011"
    println(removelzero(s))
    val s2 = "0"
    println(removelzero(s2))
    val s3 = "1111"
    println(removelzero(s3))
    val s4 = "23421414"
    val r = List(0,2,3)
    val s5 = (for(x <- s4.indices if !r.contains(x)) yield s4(x)).mkString
    println(s5)

    // 1 2 3 4 5 6 7 8 9 , k = 1 => remove 9
    // 1234567123456, k = 1 => remove 7
    println(removeKdigits("1432219", 3))
    println(removeKdigits("10200", 1))
    println(removeKdigits("10", 2))
    println(removeKdigits("112", 1))
    println(removeKdigits("10", 1))

  }
}
