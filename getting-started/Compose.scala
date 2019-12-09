object Compose extends App {

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  val f = (b: Int) => b + 1
  val g = (a: Int) => a * 100

  val c1 = compose[Int, Int, Int](f, g)
  println(c1(5))
}
