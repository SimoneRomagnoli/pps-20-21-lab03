package homework

import u03.Lists.List._
import u02.Optionals._
import u02.Optionals.Option._
import u02.SumTypes.{Person, Student, Teacher}

import scala.annotation.tailrec

object ExtendedLists {
  import u03.Lists._

  def drop[A](l:List[A])(n: Int):List[A] = l match {
    case Cons(_, t) if n > 0 => drop(t)(n-1)
    case Cons(h, t) if n <= 0=> Cons(h,t)
    case _ => Nil()
  }

  def flatMap[A,B](l:List[A])(f:A => List[B]): List[B] = l match {
    case Cons(h, t) => append(f(h), flatMap(t)(f))
    case _ => Nil()
  }

  @Override
  def map[A,B](l: List[A])(mapper: A=>B): List[B] =
    flatMap(l)(elem => Cons(mapper(elem), Nil()))

  @Override
  def filter[A](l1: List[A])(pred: A=>Boolean): List[A] = flatMap(l1)({
    case elem if pred(elem) => Cons(elem, Nil())
    case _ => Nil()
  })

  def max(l: List[Int]): Option[Int] = l match {
    case Cons(h, t) => t match {
      case Cons(_, _) => max(filter(t)(_>h))
      case Nil() => Some(h)
    }
    case _ => None()
  }

  def getCourses(l:List[Person]): List[String] = map(filter(l)(p => p.isInstanceOf[Teacher])) {
    case Teacher(_, c) => c
  }

  def getCoursesWithFlatMap(l:List[Person]): List[String] = flatMap(l){
    case Teacher(_, course) => Cons(course, Nil())
    case _ => Nil()
  }

  @tailrec
  def foldLeft[A,B](l:List[A])(initialValue:B)(op: (B,A)=>B):B = l match {
    case Cons(h,t) => foldLeft(t)(op(initialValue, h))(op)
    case Nil() => initialValue
  }

  def foldRight[A,B](l:List[A])(initialValue:B)(op: (A,B)=>B):B = l match {
    case Cons(h,t) => op(h, foldRight(t)(initialValue)(op))
    case Nil() => initialValue
  }

}