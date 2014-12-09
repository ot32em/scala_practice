sealed trait MOption[+A]
{
    def map[B](f: A => B): MOption[B] = this match {
        case MNone => MNone: MOption[B]
        case MSome(x) => MSome(f(x))
    }

    def flatMap[B](f: A => MOption[B]): MOption[B] = MNone
}
case class MSome[A](v: A) extends MOption[A]
case object MNone extends MOption[Nothing]

