package homework

import homework.ExtendedLists._
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u03.Lists.List
import u03.Lists.List.Nil
import u03.Lists.List.Cons
import u02.Optionals.Option.Some
import u02.Optionals.Option.None
import u02.SumTypes.{Person, Student, Teacher}

class TestExtendedLists {

  val list = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  val lst = Cons(10, Cons(20, Cons(30, Nil())))
  val sum = 60
  val diff = -60
  val diffRight = 20

  val people: List[Person] = Cons(Student("mario", 2021), Cons(Teacher("viroli", "fisica"), Cons(Teacher("yoshi","chimica"),Nil())))
  val courses = Cons("fisica", Cons("chimica", Nil()))

  val bools = Cons(true, Cons(true, Cons(false, Nil())))

  @Test
  def dropTest: Unit = {
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))),
      drop(lst)(0))
    assertEquals(Cons(20, Cons(30, Nil())),
      drop(lst)(1))
    assertEquals(Cons(30, Nil()),
      drop(lst)(2))
    assertEquals(Nil(),
      drop(lst)(5))
  }

  @Test
  def flatMapTest: Unit = {
    assertEquals(Cons(11, Cons(21, Cons(31, Nil ()))),
      flatMap(lst)(v => Cons(v+1, Nil())))

    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))),
      flatMap (lst)(v => Cons(v+1, Cons(v+2, Nil()))))
  }

  @Test
  def mapWithFlatMapTest: Unit = {
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))),
      map(lst)(_+1))
  }

  @Test
  def filterWithFlatMapTest: Unit = {
    assertEquals(Cons(10, Cons(20, Nil())),
      filter(lst)(_<25))

    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))),
      filter(lst)(_%10 == 0))
  }

  @Test
  def maxTest: Unit = {
    assertEquals(Some(30),
      max(lst))

    assertEquals(None(),
      max(Nil()))
  }


  @Test
  def coursesTest: Unit = {
    assertEquals(courses,
      getCourses(people))

    assertEquals(courses,
      getCoursesWithFlatMap(people))
  }

  @Test
  def foldLeftTest: Unit = {
    assertEquals(-16, foldLeft(list)(0)(_-_))


    assertEquals(sum, foldLeft(lst)(0)(_+_))
    assertEquals(diff, foldLeft(lst)(0)(_-_))
    assertEquals(true, foldLeft(bools)(false)(_||_))
    assertEquals(false, foldLeft(bools)(true)(_&&_))
  }

  @Test
  def foldRightTest: Unit = {
    assertEquals(-8, foldRight(list)(0)(_-_))

    assertEquals(sum, foldRight(lst)(0)(_+_))
    assertEquals(diffRight, foldRight(lst)(0)(_-_))
    assertEquals(true, foldRight(bools)(false)(_||_))
    assertEquals(false, foldRight(bools)(true)(_&&_))
  }


}
