/**
  * Created by lilisun on 8/21/19.
  */
object No47 {
  object Solution {
    def permuteUnique(nums: Array[Int]): List[List[Int]] = {
      solver (List((List[Int](),  nums.sorted.toList))) map (x => x._1)
    }

    def solver(l:List[(List[Int], List[Int])]):List[(List[Int], List[Int])] = {
      if(l forall (x => x._2 == Nil)) l
      else solver (l flatMap f)
    }
    def f(x:(List[Int], List[Int])):List[(List[Int], List[Int])] = x._2 match {
      case Nil => List(x)
      case _ => g(x._1)(Nil, x._2, Nil)
    }
    def g(x:List[Int])(xs1:List[Int], xs2:List[Int], acc:List[(List[Int], List[Int])]):List[(List[Int], List[Int])] = xs2 match {
      case Nil => acc distinct
      case h::t => g(x)(xs1 :+ h, t, (h::x, xs1 ++ t)::acc)
    }
  }
}
