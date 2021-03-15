package homework

import u03.Lists.List._

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

}