sealed trait Li[+A]

case object Ni extends Li[Nothing]
case class Con[+A](head: A, tail: Li[A]) extends Li[A]

object Li
{
  def apply[A](as: A*): Li[A] = 
  {
    if(as.isEmpty) Ni
    else Con(as.head, apply(as.tail: _*))
  }

  def sum(as: Li[Int]): Int = as match 
  {
    case Ni => 0
    case Con(x: Int, xs: Li[Int]) => x + sum(xs)
  }

  def product(as: Li[Double]): Double = as match
  {
    case Ni => 1.0
    case Con(0.0, _) => 0.0
    case Con(x: Double, xs: Li[Double]) => x * product(xs)
  }

  // 3.2
  def tail[A](as: Li[A]): Li[A] = as match {
    case Ni => Ni
    case Con(_, t) => t
  }

  // 3.3
  def setHead[A](as: Li[A], new_head: A): Li[A] = as match {
    case Con(a, tail) => Con(new_head, tail)
    case Ni => Ni
  }

  // 3.4
  def drop[A](as: Li[A], n: Int): Li[A] = (n, as) match {
    case (0, xs) => xs
    case (_, Ni) => Ni
    case (nn, Con(_, xs)) => drop(xs, nn - 1)
  }


  // 3.5
  def dropWhile[A](l: Li[A], f: A => Boolean): Li[A] = l match {
    case Ni => Ni
    case Con(x:A, xs:Li[A]) => if(f(x)) dropWhile(xs, f) else Con(x, dropWhile(xs, f))
  }

}

val a = Li(1,2,3)
println(Li.sum(a))

val b = Li(1.0, 2.0, 3.0, 4.0, 5.0)
println(Li.product(b))

println(b)
println(Li.tail(b))

var c = Ni
println(c)
println(Li.tail(c))


println(b)
println(Li.setHead(b, 9.9))

println(c)
println(Li.setHead(c, 1))

println(b)
println(Li.drop(b, 2))

println(c)
println(Li.drop(c, 1))


println(b)
println(Li.dropWhile(b, (x: Double) => x % 2.0 == 0.0))

