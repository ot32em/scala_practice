package mlist

trait MList[+A]
case object NList extends MList[Nothing]
case class MCons[A](a: A, as: MList[A]) extends MList[A]

object MList
{
  def apply[A](as: A*): MList[A] = 
  {
    if(as.isEmpty) NList
    else MCons(as.head, apply(as.tail: _*))
  }
}
