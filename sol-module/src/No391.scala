/**
  * Created by lilisun on 7/25/19.
  * Got TLE, this solution is straight forward but we need to scan the rectangles too many times
  * There is a O(n) solution on the discussion board. Check the following link.
  * link: https://leetcode.com/problems/perfect-rectangle/discuss/87180/O(n)-solution-by-counting-corners-with-detailed-explaination
  * I have try 2 implementation of my alo. By list and priority queue, but not lucky
  *
  */
import scala.collection.mutable
object No391 {
  object listImpl {
    // (a, b, c, d) represents matrix:
    // (a, d)      (c, d)
    //
    // (a, b)      (c, b)

    // final matrix:
    // (a min , d max ), (c max, d max)
    // (a min , b min ), (c max, b min)

    // find the matrix contains point(a min, b min, c1, d1)
    // if it's not unique, return false
    // solve rectangle cover for M0 (a min, b min, c1, d max)
    // filter all the rectangle, that in or overlap with M0.
    // if not perfect cover for M0, return false
    // update recatangles, for the overlap matrixs,
    // refine it to a new matrix, and repeat.
    // M0 <- (c1, b min, c2, d max)


    // To speed up, sort the matrix by the left down point(can be chosed as other corners)
    // When we refine a matrix, the refined matrix is sure to be the candidate of the next
    // iteration.
    //
    def isRectangleCover(A: Array[Array[Int]]): Boolean = {
      def f(l: List[Array[Int]], M: Array[Int]): Boolean = {
        println("-----")
        println(l map { x => x.toList })
        println(M.toList)
        if (zeroRectangle(M) && l.isEmpty) true
        else
          l filter { case Array(a, b, c, d) => a == M(0) && b == M(1) } match {
            case Array(a, b, c, d) :: Nil =>
              val M0 = Array(a, b, c, M.last)
              check(l takeWhile isIntersect(M0) map cut(M0), M0) match {
                case false => false
                case true =>
                  f((l takeWhile isIntersect(M0) filter overlap(M0) map refine(M0)) ++ (l dropWhile isIntersect(M0)), Array(M0(2), M(1), M(2), M.last))
              }
            case _ => false
          }
      }

      // l has a shape like
      // ????????
      // ????????
      // ????????
      // --------
      // |      |
      // |      |
      // --------
      // remove the bottom rec. and repeat
      def check(l: List[Array[Int]], M: Array[Int]): Boolean = {
        l filter { case Array(a, b, c, d) => a == M(0) && b == M(1) && c == M(2) } match {
          case Array(a, b, c, d) :: Nil =>
            f(l filterNot { case Array(a, b, c, d) => a == M(0) && b == M(1) && c == M(2) }, Array(a, d, c, M.last))
          case _ =>
            println("#####return at error")
            println(l filter { case Array(a, b, c, d) => a == M(0) && b == M(1) && c == M(2) } map { x => x.toList })
            println(M.toList)
            println("####")
            false
        }
      }

      def zeroRectangle(A: Array[Int]): Boolean = {
        A(0) == A(2) || A(1) == A(3)
      }

      // if B is interset with A
      def isIntersect(M: Array[Int])(A: Array[Int]): Boolean = {
        (M, A) match {
          case (Array(a1, b1, c1, d1), Array(a2, b2, c2, d2)) =>
            a2 < c1 && b2 < d1
        }
      }
      def refine(M: Array[Int])(A: Array[Int]): Array[Int] = {
        (M, A) match {
          case (Array(a1, b1, c1, d1), Array(a2, b2, c2, d2)) =>
            Array(a1, b2, c2, d2)
        }
      }
      def cut(M: Array[Int])(A: Array[Int]): Array[Int] = {
        (M, A) match {
          case (Array(a1, b1, c1, d1), Array(a2, b2, c2, d2)) =>
            Array(a2, b2, c1 min c2, d2)
        }
      }
      def overlap(M: Array[Int])(A: Array[Int]): Boolean = {
        (M, A) match {
          case (Array(a1, b1, c1, d1), Array(a2, b2, c2, d2)) =>
            c2 > c1
        }
      }
      def g(l: List[Array[Int]], a_min: Int, b_min: Int, c_max: Int, d_max: Int): Array[Int] = l match {
        case Nil => Array(a_min, b_min, c_max, d_max)
        case Array(a, b, c, d) :: t => g(t, a_min min a, b_min min b, c_max max c, d_max max d)
      }

      f(A.sortBy { case Array(a, b, c, d) => (a, b, c, d) }.toList,
        g(A.toList, Int.MaxValue, Int.MaxValue, Int.MinValue, Int.MinValue)
      )
    }
  }
  object pqImpl {
      def isRectangleCover(A: Array[Array[Int]]): Boolean = {
        // Initial:
        val M0 = Array(Int.MaxValue, Int.MaxValue, Int.MinValue, Int.MinValue)
        val pq = mutable.PriorityQueue[(Int, Int, Int, Int)]()(Ordering.by { case (a, b, c, d) => (-a.toInt, -b.toInt, -c.toInt, -d.toInt)})
        A.map(_.toList) foreach { case a::b::c::d::Nil =>
          M0(0) = M0(0) min a
          M0(1) = M0(1) min b
          M0(2) = M0(2) max c
          M0(3) = M0(3) max d
          pq.enqueue((a, b, c, d))
        }
        //  println("------------------")
        //pq.dequeueAll foreach println

        //true
        solvePQ(pq, M0)


      }

      def solvePQ(pq: mutable.PriorityQueue[(Int, Int, Int, Int)], M0: Array[Int]): Boolean = {
        // 1. dequeue the first element Array(a0, b0, c0, d0)
        // 2. dequeue all the element Array(a, b, c, d)
        //  satisfy a >= a0 && a < c0
        val l = pq.dequeueAll
        // println(l, M0.toList)
        l foreach {x => pq.enqueue(x)}
        if (pq.isEmpty) true
        else pq.dequeue() match {
          case (a0, b0, c0, d0) =>
            // println("first rec: ",a0, b0,c0, d0)
            //println(s"M0: ${M0.toList}")
            if (!(a0 == M0(0) && b0 == M0(1))) {
              println("####")
              println(a0,b0,c0,d0, M0.toList)
              println("####")
              return false
            }
            val subPQ = mutable.PriorityQueue[(Int, Int, Int, Int)]()(Ordering.by { case (a, b, c, d) => (-a,-b,-c,-d) })
            // optimize: only look the head element of pq, check
            if(pq.isEmpty ) return M0.toList == List(a0, b0, c0, d0)
            var h = pq.dequeue()
            while (h._1 >= a0 && h._1 < c0) {
              subPQ.enqueue((h._1, h._2, h._3 min c0, h._4))
              if (h._3 > c0) pq.enqueue((c0, h._2, h._3, h._4))
              if(pq.nonEmpty) h = pq.dequeue()
              else h = (Int.MaxValue, Int.MaxValue, Int.MinValue, Int.MinValue)
            }

            if(h._1 != Int.MaxValue) pq.enqueue(h)

            // __________________
            // |        |       |
            // |    R0  |       |
            // |--------|   R1  |
            // |    h   |       |
            // |________|_______|


            solvePQ(subPQ, Array(a0, d0, c0, M0(3))) &&
              solvePQ(pq, Array(c0, b0, M0(2), M0(3)))
        }
      }
    }
  object cornerCounter {

  }
  def main(args: Array[String]): Unit = {
    val A = Array(
      Array(1,1,3,3),
      Array(3,1,4,2),
      Array(3,2,4,4),
      Array(1,3,2,4),
      Array(2,3,3,4)
    )
   println(pqImpl.isRectangleCover(A))
  }
}
