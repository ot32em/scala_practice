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

  def sum(as: MList[Int]): Int = 
  {
    0
  }

  def tail[A](as: MList[A]): MList[A] = as match {
    case _ => as
  //  case NList => NList
  //  case MCons(x, xs) => xs
  }
}
