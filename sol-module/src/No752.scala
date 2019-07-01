/**
  * Created by lilisun on 5/15/19.
  */
object No752 {
  // out mem
  object BFSSolution {
    case class State(a:Int, b:Int, c:Int, d:Int)
    def next(str:State):List[State] = str match{
      case State(a,b,c,d) =>
        List(
          State(inc(a),b,c,d),
          State(dec(a),b,c,d),
          State(a,inc(b),c,d),
          State(a,dec(b),c,d),
          State(a,b,inc(c),d),
          State(a,b,dec(c),d),
          State(a,b,c,inc(d)),
          State(a,b,c,dec(d))
        )
    }
    def inc(x:Int):Int = if(x == 9) 0 else x + 1
    def dec(x:Int):Int = if(x==0) 9 else x -1
    def str2state(str:String):State = State(str(0) - '0', str(1) - '0', str(2) - '0', str(3) - '0')
    def openLock(deadends: Array[String], target: String): Int = {
      def notReach(set:Set[State])(s:State):Boolean = set.contains(s)
      f(Set.empty[State], List(State(0,0,0,0)), notReach( deadends map str2state toSet ), str2state(target), 0 )
    }
    def f(visited:Set[State], cur:List[State],notReach: State => Boolean, target:State, step:Int):Int = cur match {
      case Nil => -1
      case _ =>
        if(cur filterNot notReach exists (_==target)) step
        else f(visited ++ cur, cur  filterNot notReach flatMap next filterNot notReach distinct, notReach,target, step + 1)
    }
  }
  object DPSolution {
    def openLock(deadends: Array[String], target: String): Int = {
      val dp = Array.fill(10,10,10,10)(100000)
      dp(0)(0)(0)(0) = 0
      // 8 dir
      0
    }
  }
}
