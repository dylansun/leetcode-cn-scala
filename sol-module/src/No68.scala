/**
  * Created by lilisun on 2/23/19.
  */
object No68 {
  def fullJustify(words: Array[String], maxWidth: Int): List[String] = {
    var tl = 0
    var sep = List[Int]()
    for(x <- words.indices) yield {
      tl += words(x).length // Here! BUG
      if(tl > maxWidth){
        tl = words(x).length
        sep ::= x
      }
    }

    println(s"sep: $sep")
    val l = 0::sep.reverse zip (words.length::sep).reverse
    l.foldLeft(List[String]())((res, p) => res ::: List(genString(words, p._1, p._2, maxWidth)))
  }

  /**
    * TODO HERE!
    * @param words
    * @param s
    * @param t
    * @param maxwidth
    * @return
    */
  def genString( words: Array[String], s: Int, t: Int, maxwidth: Int): String ={
    val nspace =   (s until t).foldLeft(maxwidth)((sum, idx) => sum - words(idx).length)
    val nword = (s until t).length
    val s1 = if(t == words.length) nspace else nspace % nword
    val base = nspace / nword
    (s until t).foldLeft("")((res, idx) => {
      if(nspace > idx - s) res + words(idx) + (0 to base).map(x => " ").reduce(_+_)
      else res + words(idx) + (1 to base).map(x => " ").reduce(_+_)
    }
    )
  }

  def main(args: Array[String]): Unit = {
    val t1 = Array("What","must","be","acknowledgment","shall","be")
    fullJustify(t1, 16)
    println(genString(t1, 4, 6, 16))
  }
}
