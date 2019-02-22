
object No398 {

  class Solution(_nums: Array[Int]) {

    def pick(target: Int): Int =
      _nums.foldLeft((-1, 1, 0)) { case ((pickIndex, count, i), num) =>
        if (num == target) {
          if (util.Random.nextInt(count) == 0) (i, count + 1, i + 1)
          else (pickIndex, count + 1, i + 1)
        }
        else (pickIndex, count, i + 1)
      }._1
  }

}
