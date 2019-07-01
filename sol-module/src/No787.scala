object No787 {
  def dfsSearch(n: Int, flights: Array[Array[Int]], src: Int, dst: Int, k: Int): Int = {
    var ans = Int.MaxValue
    val price = Array.fill(n, n)(10000)
    val graph = Array.fill(n, n)(0)

    for{ x <- flights} {
      price(x(0))(x(1)) = x(2)
      graph(x(0))(x(1)) = 1
    }

    def dfs(node:Int, path:List[Int], cost:Int):Unit = {
      if(node == dst) {
        if(path.length <= k + 1)
          ans = ans min cost
      }
      else{
        for{
          next <- 0 until n
          if graph(node)(next) == 1
          if !path.contains(next)
          if path.length <= k+1
        } dfs(next, node::path, cost + price(node)(next))
      }

    }

    dfs(src, Nil, 0)
    if (ans == Int.MaxValue) -1 else ans
  }
  def findCheapestPrice(n: Int, flights: Array[Array[Int]], src: Int, dst: Int, k: Int): Int = {
    val dp =Array.fill(n, k+2)(  Int.MaxValue)
    for(i <- 0 to  k+1) dp(src)(i) = 0
    for(i <- 1 to k+1) {
      for(flight <- flights) {
        if(dp(flight(0))(i-1) !=  Int.MaxValue)
          dp(flight(1))(i) =dp(flight(1))(i) min (dp(flight(0))(i-1) + flight(2))
      }
    }
    if(dp(dst)(k+1) == Int.MaxValue) -1 else dp(dst)(k+1)
  }
}