sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def tail[A](l: List[A]): List[A] = l match {
    case Nil           => List()
    case Cons(_, tail) => tail
  }

  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil           => List()
    case Cons(_, tail) => Cons(h, tail)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  @annotation.tailrec
  def drop[A](n: Int, l: List[A]): List[A] = {
    if (n == 0) l
    else
      l match {
        case Cons(head, tail) => drop(n - 1, tail)
        case Nil              => Nil
      }
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(head, tail) if (f(head)) => dropWhile(tail, f)
    case _                             => l
  }

  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(input: List[A], output: List[A]): List[A] = input match {
      case Cons(h, t) if (t != Nil) => loop(t, append(output, List(h)))
      case _                        => output
    }
    loop(l, Nil)
  }

  def apply[A](l: A*): List[A] = {
    if (l.isEmpty) Nil
    else Cons(l.head, List(l.tail: _*))
  }
}

object Main extends App {
  val l1 = List(1, 2, 3, 4)
  val l2 = Nil
  println(List.tail(l1))
  println(List.tail(l2))

  println(List.setHead(100, l1))
  println(List.setHead(100, l2))

  println(List.drop(2, l1))
  println(List.drop(20, l1))
  println(List.drop(20, l2))

  println(List.dropWhile(l1, (x: Int) => x < 3))

  println(List.init(l1))
  println(List.init(l2))
}
