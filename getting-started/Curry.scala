object Curry extends App {

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)
  val c1 = curry[Int, Int, Int]((a, b) => a + b)
  println(c1(5)(6))
}
