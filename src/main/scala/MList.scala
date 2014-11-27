package mlist

trait MList[+A]
case object NList extends MList[Nothing]
case class MCons[A](head: A, tail: MList[A]) extends MList[A]

object MList
{
  def apply[A](as: A*): MList[A] = 
  {
    if(as.isEmpty) NList
    else MCons(as.head, apply(as.tail: _*))
  }

  def sum(as: MList[Int]): Int = foldRight(as, 0)(_+_)

  def product(as: MList[Double]): Double = foldRight(as, 1.0)(_*_)

  def foldRight[A](as: MList[A], a: A)(f: (A, A) => A): A = as match
  {
    case NList => a
    case MCons(x, xs) => f(x, foldRight(xs, a)(f))
  }

  def tail[A](as: MList[A]): MList[A] = as match {
    case NList => NList
    case MCons(x, xs) => xs
  }


  def setHead[A](as: MList[A], na: A): MList[A] = as match {
    case NList => NList
    case MCons(x, xs) => MCons(na, xs)
  }

  def drop[A](as: MList[A], n: Int): MList[A] = (as, n) match {
    case (NList, _) => NList
    case (xs, 0) => xs
    case (MCons(x, xs), n) => if(n < 0) MCons(x, xs) else drop(xs, n-1)
  }

  def dropWhile[A](as: MList[A], f: A => Boolean): MList[A] = as match
  {
    case NList => NList
    case MCons(x, xs) => if( f(x) ) dropWhile(xs, f) else MCons(x, dropWhile(xs, f))
  }

  def dropWhile_auto[A](as: MList[A])(f: A => Boolean): MList[A] = as match
  {
    case NList => NList
    case MCons(x, xs) => if( f(x) ) dropWhile_auto(xs)(f) else MCons(x, dropWhile_auto(xs)(f))
  }

  def append[A](as: MList[A], bs: MList[A]): MList[A] = as match {
    case MCons(x, NList) => MCons(x, bs)
    case NList => bs
    case MCons(x, xs) => MCons(x, append(xs, bs))
  }

  def init[A](as: MList[A]): MList[A] = as match {
    case NList => NList
    case MCons(x, NList) => NList
    case MCons(x, xs) => MCons(x, init(xs))
  }



}
