object Problem1 {

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): LazyList[A] = f(z) match {
    case Some((h, s)) => h #:: unfold(s)(f)
    case None => LazyList()
  }

  def pyth(n: Int): List[(Int, Int, Int)] = {
    val integers = unfold(1) {
      case x if x <= n => Some(x, x + 1)
      case _ => None
    }
    val intsList = integers.toList


    val triples = for {
      x <- intsList
      y <- intsList
      z <- intsList
    } yield(x, y, z)
    triples.filter {
      case (x, y, z) => (x * x) + (y * y) == (z * z)
    }

//    println(triples)

//    (for {
//      x <- intsList
//      y <- intsList
//      z <- intsList
//     } yield (x, y, z)).filter {
//      case (x, y, z) => (x * x) + (y * y) == (z * z)
//    }
  }


  def main(args: Array[String]): Unit = {

    println("The pythagorean triples who are at most a given limit of 10 are: "+pyth(10))
    println("The pythagorean triples who are at most a given limit of 1 are: "+pyth(1))
    println("The pythagorean triples who are at most a given limit of 5 are: "+pyth(5))
    println("The pythagorean triples who are at most a given limit of 15 are: "+pyth(15))
    println("The pythagorean triples who are at most a given limit of 25 are: "+pyth(25))

  }
}
