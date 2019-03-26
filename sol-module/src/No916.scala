/**
  * Created by lilisun on 3/25/19.
  */

object No916 {
  case class Vec(word:List[Int]){
    def +(x:Vec):Vec = Vec(add(word, x.word, Nil).reverse)
    def +(x:Char):Vec = Vec(word.slice(0, x-'a') ++ ((word(x-'a')+1)::word.slice(x-'a'+1, 26)))
    def add(l1:List[Int], l2:List[Int], acc:List[Int]):List[Int] = (l1, l2) match {
      case (h1::t1, h2::t2) => add(t1,t2, (h1 max h2)::acc)
      case _ => acc
    }
  }
  def str2Vec(str:String):Vec = str2Vec(str.toList)//Vec((0 until 26).toList.map(x => str.count(_== x + 'a')))
  def str2Vec(l:List[Char]):Vec = str2Vec(l, Vec(List.fill(26)(0)))
  def str2Vec(l:List[Char], acc:Vec):Vec = l match {
    case Nil => acc
    case h::t => str2Vec(t, acc + h)
  }
  def wordSubsets(A: Array[String], B: Array[String]): List[String] = {
    (A zip Array.fill(A.length)(merge(B.map(x => str2Vec(x)))))
      .filter(x => f(str2Vec(x._1), x._2))
      .map(_._1)
      .toList
  }

  def f(x:Vec, y:Vec):Boolean = (x.word zip y.word).forall(t => t._1 >= t._2)
  def merge(B:Array[Vec]):Vec = B.reduce(_+_)
}
