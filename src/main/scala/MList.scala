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

  def append[A](as: MList[A], bs: MList[A]): MList[A] = foldLeft(reverse(as), bs)((x, y)=>MCons(y, x))

  def init[A](as: MList[A]): MList[A] = as match {
    case NList => NList
    case MCons(x, NList) => NList
    case MCons(x, xs) => MCons(x, init(xs))
  }

  def Length[A](as: MList[A]): Int = foldLeft(as, 0)((x: Int, xs: A) => 1 + x )

  /**
   * extraction of foldLeft and foldRight
   *
   * foldLeft(cons(1, 2, 3, 4, 5, Nil), 0)(_+_)
   * foldLeft(cons(2, 3, 4, 5, Nil), 0 + 1)(f)
   * foldLeft(cons(3, 4, 5, Nil), (0 + 1) + 2)(f)
   * foldLeft(cons(4, 5, Nil), ((0 + 1) + 2) + 3)(f)
   * foldLeft(cons(5, Nil), (((0 + 1) + 2) + 3) + 4)(f)
   * foldLeft(cons(Nil), ((((0 + 1) + 2) + 3) + 4) + 5)(f)
   * ((((0 + 1) + 2) + 3) + 4) + 5)
   *
   * foldLeft(cons(2, 3, 4, 5, Nil), 1 + 0)(f)
   * foldLeft(cons(3, 4, 5, Nil), 2 + (1 + 0))(f)
   * foldLeft(cons(4, 5, Nil), 3 + (2 + (1 + 0)))(f)
   * foldLeft(cons(5, Nil), 4 + (3 + (2 + (1 + 0))))(f)
   * foldLeft(cons(Nil), 5 + (4 + (3 + (2 + (1 + 0)))))(f)
   * 5 + (4 + (3 + (2 + (1 + 0)))))

   * foldRight(cons(1,2,3,4,5,Nil), 0)(_+_)
   * 1 + foldRight(cons(2,3,4,5,Nil), 0)(f)
   * 1 + (2 + foldRight(cons(3,4,5,Nil), 0)(f))
   * 1 + (2 + (3 + foldRight(cons(4,5,Nil), 0)(f)))
   * 1 + (2 + (3 + (4 + foldRight(cons(5,Nil), 0)(f))))
   * 1 + (2 + (3 + (4 + (5 + foldRight(cons(Nil), 0)(f)))))
   * 1 + (2 + (3 + (4 + (5 + 0)))))
   *
   * foldRight(cons(2,3,4,5,Nil), 0)(f) + 1
   * (foldRight(cons(3,4,5,Nil), 0)(f)) + 2) + 1
   * ((foldRight(cons(4,5,Nil), 0)(f))) + 3) + 2) + 1
   * (((foldRight(cons(5,Nil), 0)(f)))) + 4) + 3) + 2) + 1
   * ((((foldRight(cons(Nil), 0)(f) + 5) + 4) + 3) + 2) + 1
   * ((((0 + 5) + 4)+ 3) + 2) + 1
   *
   */
  def foldRight[A, B](as: MList[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)( (b, a) => f(a, b))

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

  def reverse[A](as: MList[A]): MList[A] = foldLeft(as, NList:MList[A])((x, y) => MCons(y, x))

  def flat[A](as: MList[MList[A]]): MList[A] = 
    foldRight(as, NList:MList[A])(append(_, _))
}
