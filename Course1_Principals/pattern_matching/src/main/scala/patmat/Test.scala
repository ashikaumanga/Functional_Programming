package patmat

object Test {

  def times(chars: List[Char]): List[(Char, Int)] = {
    def iter(chars: List[Char]) : List[(Char, Int)] = {
      chars match {
        case x ::  Nil => List((x,1))
        case x :: xs => (x,1) :: iter(xs)
      }
    }
    iter(chars)
  }

  def main(args : Array[String]): Unit = {
    val r = times(List('a','b', 'c','a')).groupBy(_._1).view.mapValues( _.map(_._2).sum ).toList
    val rr = r.sortBy(a => a._2)
    println(rr)
  }

}
