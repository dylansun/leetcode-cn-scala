/**
  * 393. UTF-8 编码验证
  */
object No393 {
  def int2bin(n:Int):List[Int] = int2bin(n, Nil)
  def int2bin(n:Int, acc:List[Int] = Nil):List[Int] = n match {
    case 0 => List.fill(8 - acc.length)(0) ++ acc
    case _ => int2bin(n >> 1, (n & 1) :: acc)
  }

  def solve(l:List[List[Int]]):Boolean = l match {
    case Nil => true
    case (0::t0)::t => solve(t)
    case (1::1::0::t0)::(1::0::t1)::t => solve(t)
    case (1::1::1::0::t0)::(1::0::t1)::(1::0::t2)::t => solve(t)
    case (1::1::1::1::0::t0)::(1::0::t1)::(1::0::t2)::(1::0::t3)::t => solve(t)
    case _ => false
  }
  def validUtf8(data: Array[Int]): Boolean = {
    solve(data.toList map int2bin)
  }

  def main(args: Array[String]): Unit = {
    /*
         Char. number range  |        UTF-8 octet sequence
            (hexadecimal)    |              (binary)
         --------------------+---------------------------------------------
         0000 0000-0000 007F | 0xxxxxxx
         0000 0080-0000 07FF | 110xxxxx 10xxxxxx
         0000 0800-0000 FFFF | 1110xxxx 10xxxxxx 10xxxxxx
         0001 0000-0010 FFFF | 11110xxx 10xxxxxx 10xxxxxx

         0~127
         192 + (0 ~ 63)
         256 + (0 ~ 15)
         288 + (0 ~ 7 )
     */
    println(1,2,4,8,16,32,64,128)
  }
}
