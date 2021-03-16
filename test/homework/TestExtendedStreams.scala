package homework

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u03.Lists.List.Cons
import u03.Lists.List.Nil
import u03.Streams.Stream

class TestExtendedStreams {

  val s = Stream.take(Stream.iterate(0)(_+1))(10)
  val x = "x"

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

}
