/**
  * Created by lilisun on 3/19/19.
  */
object No945 {
  def minIncrementForUnique(A: Array[Int]): Int = minIncrementForUnique(A.sorted, 0)
  // A is sorted in increasing order
  def solver(A:Array[Int]):Int = {
    var acc = 0
    var i = 0
    while(i < A.length){
      var n = 0
      var j = i+1
      while(j < A.length && A(j) == A(i)){n +=1; j+=1}
      if(i+n+1 == A.length) return acc + (n+1)*n / 2
      val b = A(i+n+1); val k = b - A(i)
      k > n match {
        case true => {
          i += n +1
          acc += (n+1)*n / 2
        }
        case _ =>{
          for(j <- i+k to i+n) {
            A(j) = b
          }

          i += k
          acc += (k-1)*k/2 + (n-k+1)*k
        }
      }
    }
    acc
  }
  def minIncrementForUnique(A: Array[Int], acc: Int): Int = {
    A.length match {
      case 0 => acc
      case _ => {
        val n = countHead(A.tail, A.head, 0)
        // (a, a , a, a.., a) => (a+0, a+1, ..., a+n) => (a+0,..,a+k-1, a+k,..,a+k, a+k)
        // b = a + k
        //
        if(n+1 == A.length) return acc + (n+1)* n / 2
        val b = A(n+1); val k = b - A.head
        k > n  match {
          case true => minIncrementForUnique( A.slice(n+1, A.length), (n+1)* n / 2 + acc)
          case false => {
            //
            val newA = A.slice(k, n+1).map(x => b) ++ A.slice(n+1, A.length)
            minIncrementForUnique(newA, acc + (k-1)*k/2 + (n-k+1)*k)
          }
        }

      }
    }
  }

  def countHead(A:Array[Int],head:Int, acc:Int):Int = {
    if(A.isEmpty || A.head != head) acc
    else countHead(A.tail, head, acc+1)
  }

  def main(args: Array[String]): Unit = {
    val test = (1 to 100000).toList.reverse.toArray
    val t1 = System.currentTimeMillis()
    println(solver(test.sorted)) // 50000
    val t2 = System.currentTimeMillis()
    println(s"${t2-t1}")
  }
}
