package homework

import scala.annotation.tailrec

object ExtendedStreams {
  import u03.Streams._
  import u03.Streams.Stream.{Cons, Empty, cons, empty}

  @tailrec
  def drop[A](s: Stream[A])(n:Int):Stream[A] = s match {
    case Cons(_, t) if n > 0 => drop(t())(n-1)
    case Cons(h, t) if n <= 0 => cons(h(), t())
    case _ => empty()
  }

  def constant[A](init: A): Stream[A] = Stream.iterate(init)(elem=>elem)

  def fib(n:Int):Int = {
    @annotation.tailrec
    def _fib(n:Int, first:Int, second:Int): Int = n match {
      case 0 => first
      case 1 => second
      case _ => _fib(n-1, second, first+second)
    }
    _fib(n, 0, 1)
  }

}
