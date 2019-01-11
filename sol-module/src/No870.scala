import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap
object No870 {

  def advantageCount(A: Array[Int], B: Array[Int]): Array[Int] = {
    val B_sorted = B.sorted
    val A_best = advantageCountSorted(A.sorted, B_sorted)
    reconstruct(A_best, B_sorted, B)
  }

  def reconstruct( A_best: Array[Int], B_sorted: Array[Int], B_original: Array[Int]): Array[Int] = {
    val hash_map = HashMap[Int, Stack[Int]]()
    val n = A_best.length

    // duplicate elements
    for(i <- 0 to n-1){
      val key = B_sorted(i)
      val value = A_best(i)
      if(hash_map.contains(key)) hash_map.get(key).get.push(value)
      else hash_map.put(key, new Stack[Int]().push(value))
    }
    (for(i <- 0 to n -1 ) yield hash_map.get(B_original(i)).get.pop()).toArray
  }

  //A and B in increasing order
  def advantageCountSorted(A: Array[Int], B: Array[Int]): Array[Int] ={

    val n = A.length
    val ans = (-n to -1).toArray
    var tail = n - 1
    var j = 0
    for(i <- 0 to A.length -1){
     if(A(i) > B(j)){ans(j) = A(i); j=j+1}
     else {ans(tail) = A(i);tail = tail - 1}
    }
    ans
  }

  def main(args: Array[String]): Unit = {
    val A = Array[Int](12,24,8,32, 8, 8, 9 , 9)
    val B = Array[Int](13,25,32,11, 7, 7, 4, 3)
    val C = advantageCount(A, B)
    println(C.mkString)
  }
}
