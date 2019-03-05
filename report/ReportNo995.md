#### No 995 Minimum Number of K Consecutive Bit Flips
1. 贪心算法。从左到右边: 遇到1,skip; 遇到0, 翻转。

```Scala
  object No995 {
      def minKBitFlips(A: Array[Int], K: Int): Int = minKBitFlips(A,K,0)
      def minKBitFlips(A: Array[Int], K: Int, acc: Int): Int = {
       // println(s"A: ${A.mkString("[", ",", "]")}, acc: $acc")
        if(K == A.length) {
          if(A.mkString == A.map(x => 1).mkString)  acc
          else if(A.mkString == A.map(x => 0).mkString)  1+acc
          else -1
        }else{
          if(A(0) == 1) minKBitFlips(A.tail, K, acc)
          else minKBitFlips(flip(A.tail, K), K, acc + 1)
        }
      }

      def flip(A: Array[Int], K: Int): Array[Int] = {
        val B = A
        for(x <- 0 until K - 1) B(x) = if(B(x) == 1) 0 else 1
        B
      }
  }
```

2.优化: 修改` minKBitFlips(flip(A.tail, K), K, acc + 1)`  直接进行翻转操作会TLE, 故只需要记录翻转次数,但是记录翻转次数和直接进行翻转的时间代价是一样的。
所以, 我们必须再次根据翻转操作的特殊性来减少时间开销。

翻转分析: 如果A(0) == 0, 那么翻转的范围是A(1),...,A(K-1),
接下来需要判断 A(1) 是否为 1。

在不翻转数组的情况,需要记录当前的翻转状态flap。

举一个特例: (0,0,0,1,0,0,0), k=3

起始状态: flag =0,

对第一个元素进行操作, flag = 1

第二个元素 为0 , 这是flag = 1, 表明已经翻转过了, 所以 0 不需要翻转

当到达地第4个元素为1的时候, 我们发现这时候, 需要额外的信息告诉我们 第4个点没有被翻转过,

这时候需要记录h(3) = 1, 并利用h(3) 来重置 flag 。

么重置之后, flag 变为 0, A(3)=1, 则不需要翻转。 那么重复这些步骤即可。

```Scala
  def minKBitFlips(A: Array[Int], K: Int): Int = solverOptGreedy(A, K)

  def solverOptGreedy(A: Array[Int], K: Int): Int = {
    val N = A.length
    val hint = (0 until N).map(x => 0).toArray
    var ans = 0; var flip = 0

    for (i <- 0 until N) {
      flip ^= hint(i)
      if (A(i) == flip) {
        ans+= 1
        if (i + K > N) return -1
        flip ^= 1
        if (i + K < N) hint(i + K) ^= 1
      }
    }
    ans
  }
```


