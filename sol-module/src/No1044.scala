object No1044 {

  /***
    * Here is a binary solution from lee215. [python]
    * Todo:
    class Solution:
      def longestDupSubstring(self, S):
          A = [ord(c) - ord('a') for c in S]
          mod = 2**63 - 1

          def test(L):
              p = pow(26, L, mod)
              cur = reduce(lambda x, y: (x * 26 + y) % mod, A[:L], 0)
              seen = {cur}
              for i in xrange(L, len(S)):
                  cur = (cur * 26 + A[i] - A[i - L] * p) % mod
                  if cur in seen: return i - L + 1
                  seen.add(cur)
          res, lo, hi = 0, 0, len(S)
          while lo < hi:
              mi = (lo + hi + 1) / 2
              pos = test(mi)
              if pos:
                  lo = mi
                  res = pos
              else:
                  hi = mi - 1
          return S[res:res + lo]
    */
  object Solution{

  }

  // TODO: optimize
  //
  object SolutionSuffixTie {
    class Trie(){
      val next = Array.ofDim[Trie](26)
      var size = 0
      def insert(s:String):Unit = {
        var node:Trie = this
        for(ch <- s){
          if(node.next(ch - 'a') == null) node.next(ch-'a') = new Trie()
          node.size += 1
          node = node.next(ch - 'a')
        }
        node.size += 1
      }
      def findMax():String = {
        var ans = ""
        var l = this.next.zipWithIndex zip Array.fill(26)("") map {case ((trie, idx), path) => (trie, path + (idx + 'a').toChar)} filter (_._1 != null)
        def f(node:(Trie, String)):Array[(Trie, String)] = {
          if(node._2.length > ans.length) ans = node._2
          node._1.next.zipWithIndex
            .filter {case (trie, idx) => trie != null && trie.size > 1}
            .map {case (trie, idx) => (trie, node._2 + (idx + 'a').toChar)}
        }
        while(l exists { case (trie, path) => trie != null && trie.size > 1}) l = l flatMap f
        ans
      }
    }
    def longestDupSubstring(s: String): String = {
      val trie = new Trie()
      for{i <- s.indices} trie.insert(s.substring(i))
      trie.findMax()
    }

  }
}
