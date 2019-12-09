object Uncurry extends App{
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  val c1 = uncurry[Int, Int, Int](a => b => a + b)
  println(c1(7, 6))
}
