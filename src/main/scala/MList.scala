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

  def sum(as: MList[Int]): Int = as match
  {
    case NList => 0
    case MCons(x, xs) => x + sum(xs)
  }

  def tail[A](as: MList[A]): MList[A] = as match {
    case NList => NList
    case MCons(x, xs) => xs
  }

  def product(as: MList[Double]): Double = as match {
    case NList => 1.0
    case MCons(0.0, _) => 0.0
    case MCons(x, xs) => x * product(xs)
  }

  def setHead[A](as: MList[A], na: A): MList[A] = as match {
    case NList => NList
    case MCons(x, xs) => MCons(na, xs)
  }
}
