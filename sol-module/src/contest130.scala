/**
  * Created by lilisun on 3/31/19.
  */
object contest130 {
  def prefixesDivBy5(A: Array[Int]): Array[Boolean] = {
    f(A.toList, 0, Nil).toArray
  }
  def f(A:List[Int], rem: Int, acc:List[Boolean]):List[Boolean] = A match {
    case h::t => f(t, (rem *2 + h) % 5,  ((rem *2 + h) % 5 == 0)::acc)
    case Nil => acc.reverse
  }

  def main(args: Array[String]): Unit = {
    println(baseNeg2(22))
  }
  // x = a0 * 2^0 + .... a(n-1_ * 2^n-1
  // y = b0 * (-2)^0 + .... + b(
  def baseNeg2(N: Int): String = {
    if(N == 0) "0" else
    rml0(help(addf(base(4)(N, Nil), 0 , Nil:List[Int]).reverse)).mkString
  }
  def help(l:List[Int], acc:List[Int] = Nil):List[Int] = l match {
    case Nil => acc
    case 2::Nil => 1::1::0::acc
    case 2::0::Nil => 1::1::0::acc
    case 2::1::t => help(t, 0::0::acc)
    case 2::2::t => help(t, 1::0::acc)
    case 2::0::0::t => help(t, 1::1::0::acc)
    case 2::0::1::t => help(2::t, 1::0::acc)
    case h::t => help(t, h::acc)
  }
  def rml0(l:List[Int]):List[Int] = l match {
    case 0::t => rml0(t)
    case _ => l
  }
  // 0 = 0 * (-2)^2 + 0 * (-2) + 0 * (-2)
  // 1 = 0,0, 1
  // 2 = 1,1, 0
  // 3 = 1,1, 1
  def addf(l:List[Int], dn:Int, acc:List[Int]):List[Int] = {
    println(l, dn, acc)
    l match {
      case Nil => acc ++ List(dn)
      case h::t => h match {
        case 0 => addf(t, 0, acc ++ List(0 + dn,0))
        case 1 => addf(t, 1, acc ++ List(0 + dn,0))
        case 2 => addf(t, 0, acc ++ List(1 + dn,1))
        case 3 => addf(t, 1, acc ++ List(1 + dn,1))
      }
    }
  }
  def base(n:Int)(N:Int, acc:List[Int] = Nil):List[Int] = {
    if(N == 0) acc
    else base(n)(N/n, (N%n) :: acc)
  }

  def numEnclaves(A: Array[Array[Int]]): Int = {
    var tmp = A.clone
    var queue = List[(Int, Int)]()
    //
    // -------
    // |     |
    // -------
    for{j <- A(0).indices
        i <- Set(0, A.length -1)
        if A(i)(j) == 1
    } queue ::= (i, j)
    for{i <- 1 to A.length - 2
        j <- Set(0, A(0).length - 1)
        if A(i)(j) == 1
    } queue ::= (i,j)

    val dir = List((0,1), (1,0), (-1,0), (0,-1))

    while(queue.nonEmpty){
      // printMap(tmp)
      val t = queue.head
      queue = queue.tail
      // println("current pos ", t)
      tmp(t._1)(t._2) = 0
      dir.foreach{ d =>
        val npos = (t._1 + d._1, t._2 + d._2)
        //println("pos", npos)
        if(tmp.indices.contains(npos._1) && tmp(npos._1).indices.contains(npos._2) && tmp(npos._1)(npos._2) == 1)
          queue ::= npos
      }
      println("queue",queue)
    }
    tmp.map(_.sum).sum
  }

  def printMap(m: Array[Array[Int]]):Unit = {
    m.foreach(x => println(x.mkString(" ")))
    println("----------------------------")
  }
}
