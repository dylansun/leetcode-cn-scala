import scala.collection.mutable.Stack
object No39 {
  /**
    *
    * @param candidates: unique integer > 0
    * @param target: > 0
    * @return
    */
  def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
    if(candidates.length <= 0 || target < 0) return Nil

    dfs(candidates.sorted.reverse , target, 0,  Stack[Int]() , List[List[Int]]())
  }

  def dfs(candidates: Array[Int], target: Int, start: Int, path: Stack[Int],res: List[List[Int]]):List[List[Int]] ={

    if(target == 0) return  path.toList :: res
    if(target < 0)  return Nil

    var tmp = res

    for(i <- start to candidates.length - 1  if target >= candidates(i)){
        path.push(candidates(i))
        tmp =  dfs(candidates, target - candidates(i), i, path, res) ::: tmp
        path.pop()
    }
    tmp
  }

  def main(args: Array[String]): Unit = {
    /**
    val l1 = List(1, 2 ,4)
    val l2 = List(1, 3, 5)
    val l3 = l1::(l2)::Nil
    val l4 = List(2,4,6)
    val l5 = l3::l4::Nil
    val l6 = l4::l3
    val l7 = l3:::l6
    println(l1.mkString)
    println(l3)
    println(l5)
    println(l7)
      **/


    val candidates = Array(2,3,5)
    val target = 8
   val res =  combinationSum(candidates, target)
    println(res)
  }




}
