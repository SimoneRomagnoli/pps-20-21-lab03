package homework

import homework.ExtendedStreams._
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u03.Lists.List.Cons
import u03.Lists.List.Nil
import u03.Streams.Stream

class TestExtendedStreams {

  val s = Stream.take(Stream.iterate(0)(_+1))(10)
  val x = "x"
  val fibs:Stream[Int] = Stream.map(Stream.iterate(0)(_+1))(fib)

  @Test
  def dropTest: Unit = {
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))),
      Stream.toList(ExtendedStreams.drop(s)(6)))
  }

  @Test
  def constantTest:Unit = {
    assertEquals(Cons(x, Cons(x, Cons(x, Cons(x, Cons(x, Nil()))))),
      Stream.toList(Stream.take(ExtendedStreams.constant(x))(5)))
  }

  @Test
  def fibonacciTest:Unit = {
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))),
      Stream.toList(Stream.take(fibs)(8)))
  }

}
