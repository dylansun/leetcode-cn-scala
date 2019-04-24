/**
  * Created by lilisun on 4/19/19.
  */
import scala.language.postfixOps
object No140 {
  case class Part(l:List[String], rest:List[Char])
  def wordBreak(s: String, wordDict: List[String]): List[String] = {
    val bound = (wordDict map {x => x.length}).max
    solver (wordDict, bound) (List(Part(Nil, s.toList))) map (x => x.l.mkString(" "))
  }
  def solver(d:List[String], b:Int)(l:List[Part], acc:List[Part] = Nil):List[Part] = l match {
    case Nil => acc
    case _ => if(l exists  {x => x.rest == Nil}) solver(d, b)(l filter (x => x.rest != Nil), acc ++ (l filter (x => x.rest == Nil)))
      else solver(d,b)(l flatMap f(d, b), acc)
  }

  def f(dict:List[String], bound:Int)(part:Part) = {
    for{
      i <- 1 to (bound min part.rest.length)
      word = part.rest.slice(0, i).mkString
      if dict contains word
    } yield Part(part.l :+ word, part.rest.slice(i, part.rest.length))
  }

  def main(args: Array[String]): Unit = {
    val s = "catsanddog"
    val dict = List("cat","cats","and","sand","dog")
    wordBreak(s,dict).foreach(println)
  }
}
