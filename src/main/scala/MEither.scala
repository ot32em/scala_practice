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

    def map2[EE >: E, B, C](b: MEither[EE, B])(f: (A, B) => C): MEither[EE, C] =
        this flatMap (aa => b map (bb => f(aa, bb)))
}
case class MRight[+A](v: A) extends MEither[Nothing, A]
case class MLeft[+E](e: E) extends MEither[E, Nothing]

object MEither
{
    def lift2[A, B, C, E](f: (A, B) => C): (MEither[E, A], MEither[E, B]) => MEither[E, C] =
        (ea: MEither[E, A], eb: MEither[E, B]) => ea flatMap ( a => eb map ( b => f(a, b)))

    def sequence[E, A](as: List[MEither[E, A]]): MEither[E, List[A]] =
        traverse(as)(v => v)

    def traverse[E, A, B](as: List[A])(f: A => MEither[E, B]): MEither[E, List[B]] =
        as.foldLeft(
            MRight(List()): MEither[E, List[B]]
        )(
            (ez: MEither[E, List[B]], a: A) => lift2((z: List[B], b: B) => z :+ b)(ez, f(a))
        )
}
