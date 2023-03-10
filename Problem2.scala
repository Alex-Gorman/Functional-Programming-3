object Problem2 {

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): LazyList[A] = f(z) match {
    case Some((h, s)) => h #:: unfold(s)(f)
    case None => LazyList()
  }

  /* 1. Function to construct a list of factors */
    def factors(n: Int): LazyList[Int] = {
    val integers = unfold(1) {
      case x if x < n => Some(x, x+1)
      case _ => None
    }
    integers.filter(x => n % x == 0)
  }

  /* 2. Funcion to get the sum of the factors  */
  def sum(lst: LazyList[Int]): Int = {
    lst.foldRight(0)(_+_)
  }


  /* 3. Function creating LazyList using unfold, filter and (1) & (2) functions */
  def perfectNum(n: Int): LazyList[Int] = {
    val integers = unfold(1) {
      case x if x < n => Some(x, x+1)
      case _ => None
    }

    integers.filter(x => x == sum(factors(x)))
  }

  def main(args: Array[String]): Unit = {
    println("The first 2 perfect number are: ")
    println(perfectNum(500).take(2).toList)

    println("The first 4 perfect numbers up to 10,000 are: ")
    println(perfectNum(1000000000).take(4).toList)
  }

}
