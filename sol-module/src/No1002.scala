import scala.collection.mutable
object No1002 {
  def commonChars(A: Array[String]): List[String] = {
    if(A.length == 0) return List[String]()
    if(A.length == 1) return A(0).toCharArray.map(x => x.toString).toList
    val m = mutable.HashMap[Char,Int]()
    val keys = A.foldLeft(('a' to 'z').toSet)( (keys, x) => x.toCharArray.toSet intersect keys )
    for(x <- A.head.toCharArray if keys.contains(x)) m.put(x, m.getOrElse(x,0)+1)
    A.tail.foreach(str => for(key <- m.keySet)m(key) = m(key) min str.count(_==key))

    var ans = List[String]()
    for(k <- m.keySet) for(i <- 0 until m(k)) ans = k.toString::ans
    ans
  }
}
