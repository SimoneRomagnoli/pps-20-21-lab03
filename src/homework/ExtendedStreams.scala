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

  

}
