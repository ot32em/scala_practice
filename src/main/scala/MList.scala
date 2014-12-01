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

  def sum(as: MList[Int]): Int = foldLeft(as, 0)(_+_)

  def product(as: MList[Double]): Double = foldLeft(as, 1.0)(_*_)


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
    case MCons(x, xs) => 
      if(f(x)) 
        dropWhile(xs, f) 
      else 
        MCons(x, dropWhile(xs, f))
  }

  def dropWhile_auto[A](as: MList[A])(f: A => Boolean): MList[A] = as match
  {
    case NList => NList
    case MCons(x, xs) => if( f(x) ) dropWhile_auto(xs)(f) else MCons(x, dropWhile_auto(xs)(f))
  }

  def append[A](as: MList[A], bs: MList[A]): MList[A] = as match {
    case NList => bs
    case MCons(x, NList) => MCons(x, bs)
    case MCons(x, xs) => MCons(x, append(xs, bs))
  }

  def init[A](as: MList[A]): MList[A] = as match {
    case NList => NList
    case MCons(x, NList) => NList
    case MCons(x, xs) => MCons(x, init(xs))
  }

  def Length[A](as: MList[A]): Int = foldLeft(as, 0)((x: Int, xs: A) => 1 + x )

  def foldRight[A, B](as: MList[A], b: B)(f: (A, B) => B): B = as match
  {
    case NList => b
    case MCons(x, xs) => f(x, foldRight(xs, b)(f))
  }

  def foldRight2[A, B](as: MList[A], b: B)(f: (A, B) => B): B =  b
  /**
   * foldright(A[1,2,3,4,5], 0)(_+_)
   * 1 + foldRight(A[2,3,4,5], 0)
   * 1 + 2 + foldRight(A[3,4,5], 0)
   * 1 + 2 + 3 + foldRight(A[4,5], 0)
   * 1 + 2 + 3 + 4 + foldright(A[5], 0)
   * 1 + 2 + 3 + 4 + 5 + foldRight(NList, 0)
   * 1 + 2 + 3 + 4 + 5 + 0
   *
   *
   */


  @annotation.tailrec
  def foldLeft[A, B](as: MList[A], b: B)(f: (B, A) => B): B = as match
  {
    case NList => b
    case MCons(x, xs) => foldLeft(xs, f(b, x))(f)
  }

  def reverse[A](as: MList[A]): MList[A] = 
    foldLeft( as, NList: MList[A] )( 
      (xs: MList[A], x: A) => MCons(x, xs)
    )
    
  

}
