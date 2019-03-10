/**
  * Created by lilisun on 3/10/19.
  */
object No717 {
  def isOneBitCharacter(bits: Array[Int]): Boolean = {
      conseqOnesFromTail(bits.dropRight(1)) % 2 == 0
  }

  def conseqOnesFromTail(bits: Array[Int]): Int = {
    var c = 0
    for( idx <- bits.indices.reverse){
      bits(idx) match {
        case 0 => return c
        case 1 => c += 1
      }
    }
    c
  }

  def main(args: Array[String]): Unit = {
    val bits = Array(0,1,1,0,1,1,1,0)
    println(bits.dropRight(1).scanRight(0)(_+_).toList)
  }
}
