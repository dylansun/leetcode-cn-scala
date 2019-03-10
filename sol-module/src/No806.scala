/**
  * Created by lilisun on 3/9/19.
  */
object No806 {
  def numberOfLines(widths: Array[Int], S: String): Array[Int] = {
    S
      .map( x => widths(x - 'a'))
      .foldLeft(Array(0,0))( (res, x) => {
        if(x + res(0) > 100) Array(res(0)+1, x)
        else Array(res(0), res(1) + x)
      })
      .array

  }
}
