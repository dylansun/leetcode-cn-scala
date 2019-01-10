/**
  * Created by lilisun on 1/10/19.
  */
object No851 {
  def loudAndRich(richer: Array[Array[Int]], quiet: Array[Int]): Array[Int] = {

    // Initial people matrix
    val n = quiet.length
    val p_mat = Array.ofDim[Boolean](n, n)
    for(i <- 0 to richer.length - 1){
      p_mat(richer(i)(0))(richer(i)(1)) = true
    }

    //
    val answer: Array[Int] = ( - n to -1).toArray
    println(answer)
    def dfs(node: Int): Int ={
      //Want least quiet person in this subtree
      if(answer(node) < 0 ){
        answer(node) = node
        for(i <- 0 to n-1 if p_mat(i)(node)){
          val cand = dfs(i)
          if(quiet(cand) < quiet(answer(node)))
            answer(node) = cand
        }
      }

      return answer(node)
    }


    return (for(i <- 0 to n-1) yield dfs(i)).toArray
  }
}

  def main(args: Array[String]): Unit = {
    println("")
  }
}
