/**
  * Created by lilisun on 3/17/19.
  */
object No1012 {
  def bitwiseComplement(N: Int): Int = {
    val len = lenBit(N)
    Math.pow(2, len).toInt -1 -N
  }
  def lenBit(N:Int):Int = {
    var len = 1
    var n = N /2
    while(n != 0){
      n = n / 2
      len +=1
    }
    len
  }
}
