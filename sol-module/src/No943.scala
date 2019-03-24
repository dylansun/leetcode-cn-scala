/**
  * Created by lilisun on 3/22/19.
  */
object No943 {

 // Travelling Salesman Problem

 // graph[i][j] means the length of string to append when A[i] followed by A[j]. eg. A[i] = abcd, A[j] = bcde, then graph[i][j] = 1
 // Then the problem becomes to: find the shortest path in this graph which visits every node exactly once. This is a Travelling Salesman Problem.
 // Apply TSP DP solution. Remember to record the path.
 def shortestSuperstring(A: Array[String]): String = {
  val graph = buildGraph(A, cost)
  val n = A.length
  val dp= Array.fill(n, 1<<n)(100000)
  val path = Array.fill(n, 1<<n)(List[Int]())
  for(j <- 1 until 1<<n){
    println(j)
   for(i <- 0 until n){
    if(((j >> i) & 1 )== 1){
     if((j - (1<<i)) == 0)
      {
       dp(i)(j)= A(i).length
       path(i)(j) = List(i)
      }
     else{
      for(k <- 0 until n if ((j>>k) & 1) == 1 && k != i){
        //println(s"$k dp(i,j) ${dp(i)(j)}   = dp(i,i,k) ${dp(i)((1<<k) + (1<<i))} + dp(k, j - i)${dp(k)(j - (1 << i))}")
        if(dp(i)(j) > graph(k)(i) + dp(k)(j - (1 << i))){
         dp(i)(j)   = graph(k)(i) + dp(k)(j - (1 << i))
         path(i)(j) = path(k)(j - (1 << i)) ::: List(i)
        }
      }
     }
    }
   }
  }

   val mind = (0 until n)
     .map( x => dp(x)((1<<n) -1))
     .zipWithIndex
     .sortBy(x => x._1)
     .head._2

   val p = path(mind)((1<<n) -1)
   println(dp(mind)((1<<n) -1),p)
   generateAns(A, p, graph)
 }

  def generateAns(A:Array[String], path: List[Int], graph: Array[Array[Int]]):String = {

    A(path.head) + (path zip path.tail)
      .map( x => A(x._2).substring(A(x._2).length-graph(x._1)(x._2), A(x._2).length)).mkString
  }
 def buildGraph(A:Array[String], f: (String, String) => Int):Array[Array[Int]] ={
  val graph = Array.fill(A.length, A.length)(0)
  for(i <- A.indices){
   for(j <- A.indices if j!=i){
    graph(i)(j)=f(A(i), A(j))
   }
  }
  graph
 }

 def cost(s1:String, s2:String): Int = {
  var m = s1.length
  for(i<- 0 until s1.length){
   if(s2.startsWith(s1.substring(i,s1.length))){
     m = m min i
   }
  }
   s2.length - s1.length + m
 }

 def main(args: Array[String]): Unit = {
    val A = Array("catg","ctaagt","gcta","ttca","atgcatc")
   val ans = "gctaagttcatgcatc"
   val as = "atgcatcatgctaagttca"
    val graph = buildGraph(A, cost)
   val path = List(2,1,3,4)
   println(generateAns(A, path, graph) == ans)
   graph.foreach(x => println(x, " : ", x.toList))
   println(ans.length)
   println(shortestSuperstring(A))
 }


}
