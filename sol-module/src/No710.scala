/**
  * Created by lilisun on 2/16/19.
  */

class No710(_N: Int, _blacklist: Array[Int])  {
  val candidate = (0 to _N).filter(x => !_blacklist.contains(x))
  def pick(): Int = {
    candidate( (System.currentTimeMillis() % candidate.size).toInt)
  }
}
