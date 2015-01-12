trait MEither[+E, +A] {
    def map[B](f: A => B): MEither[E, B] = this match {
        case MRight(v) => MRight(f(v))
        case MLeft(err) => MLeft(err)
    }

    def flatMap[EE >: E, B](f: A => MEither[EE, B]): MEither[EE, B] = this match {
        case MRight(v) => f(v)
        case MLeft(er) => MLeft(er)
    }

    def orElse[EE >: E, B >: A](b: => MEither[EE, B]): MEither[EE, B] = this match {
        case MRight(v) => this
        case MLeft(err) => b
    }

    def map2[EE >: E, B, C](b: MEither[EE, B])(f: (A, B) => C): MEither[EE, C] = null
}
case class MRight[+A](v: A) extends MEither[Nothing, A]
case class MLeft[+E](e: E) extends MEither[E, Nothing]

object MEither
{
    def sequence[E, A](as: List[MEither[E, A]]): MEither[E, List[A]] = null
    def traverse[E, A, B](as: List[MEither[E, A]])(
                          f: A => MEither[E, B]): MEither[E, List[B]] = null
}
