/**
  * Created by lilisun on 5/9/19.
  */
object No638 {
  object Solution {
    case class State(needs:List[Int], cost:Int){
      def isdone():Boolean = needs forall (_==0)
      def calCost(price:List[Int]):Int = {
        (needs zip price map {case(x, y) => x * y}).sum + cost
      }
    }
    def shoppingOffers(price: List[Int], special: List[List[Int]], needs: List[Int]): Int = {
      def f(state:State):List[State] = {
        for{
          sp <- special
          nstate = state.needs zip sp map{case (x, y) => x - y}
          if nstate forall (_ >=0)
        } yield State(nstate, state.cost + sp.last)
      }
      def help(l:List[State], acc:Int):Int = l match {
        case Nil => acc
        case _ =>
          (l flatMap f) match {
            case Nil => acc min (l map (_.calCost(price))).min
            case h::t => help(h::t,  acc min (l map (_.calCost(price))).min)
          }
      }
      help(List(State(needs, 0)), Int.MaxValue)
    }
  }
}
