/**
  * Created by lilisun on 5/23/19.
  */
object No432 {
  class AllOne() {

    /** Initialize your data structure here. */
    // group key with same value
    //  g1 : key1 , key 2, .. keyn where key 1, key 2 == 1
    // inc key, if not exist insert the g1 else
    // find gi, and move key i to key i + 1

    val mem = scala.collection.mutable.HashMap[String, Int]()
    var max_value:Option[Int] = None
    var min_value:Option[Int] = None
    val value_str = scala.collection.mutable.HashMap[Int, Set[String]]()

    /** Inserts a new key <Key> with value 1. Or increments an existing key by 1. */
    def inc(key: String) {
      mem.put(key, 1 + (mem getOrElse (key, 0)))
      if(mem(key) == 1) {
        max_value match {
          case None =>
            max_value = Some(1)
            value_str.put(1, Set(key))
          case Some(x) => {}
        }
        min_value match {
          case None =>
            min_value = Some(1)
          case Some(x) => min_value = Some(1)
        }
        value_str.put(1, value_str.getOrElse(1, Set.empty[String]) + key)
      }else{
        value_str.put(mem(key)-1, value_str(mem(key)-1) - key)
        if(value_str(mem(key) -1).isEmpty) value_str -= mem(key) -1
        value_str.put(mem(key), value_str.getOrElse(mem(key), Set.empty[String]) + key)
        if(mem(key) > max_value.get){
          max_value = Some(mem(key))
          println(s"inc $key, ${mem(key)}")
        }
        if(mem(key) - 1 == min_value.get){
          if(!value_str.contains(mem(key) -1)){
            min_value = Some(mem(key))
          }
        }
      }
      show
    }

    /** Decrements an existing key by 1. If Key's value is 1, remove it from the data structure. */
    def show():Unit = {
      println(s"${max_value}, ${min_value}, ${mem}, ${value_str}")
    }
    def dec(key: String) {
      if(!mem.contains(key)) return
      mem.put(key, - 1 + (mem getOrElse (key, 0)))
      if(mem (key) <= 0) mem -= key
      if(mem.contains(key)){
        //println("hello")
        //show
        value_str.put(mem(key) + 1, value_str(mem(key) + 1) - key)
        if(value_str(mem(key) +1).isEmpty) value_str -= (mem(key) + 1)

        value_str.put(mem(key), value_str.getOrElse(mem(key), Set.empty[String]) + key)
        if(max_value.get == mem(key) + 1 && !value_str.contains(mem(key) + 1)) max_value = Some(mem(key))
        if(mem(key) < min_value.get) min_value = Some(mem(key))

      }else{
        value_str.put(1, value_str(1) - key)
        if(value_str(1).isEmpty) value_str - 1

        if(max_value.get == 1 && value_str(1).isEmpty) max_value = None
        // for sure min_value must be Some(1)
        if(value_str(1).isEmpty) min_value = value_str.keySet.filter(x => value_str(x).nonEmpty).toList.sorted.headOption
      }
      show
    }

    /** Returns one of the keys with maximal value. */
    def getMaxKey(): String = {
      show
      max_value match {
        case Some(x) => value_str(x).head
        case None => ""
      }
    }

    /** Returns one of the keys with Minimal value. */
    def getMinKey(): String = {
      show
      min_value match {
        case Some(x) => value_str(x).head
        case None => ""
      }
    }
  }

  /**
    * Your AllOne object will be instantiated and called as such:
    * var obj = new AllOne()
    * obj.inc(key)
    * obj.dec(key)
    * var param_3 = obj.getMaxKey()
    * var param_4 = obj.getMinKey()
    */
}
