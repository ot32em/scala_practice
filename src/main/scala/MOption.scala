sealed trait MOption[+A]
{
    def map[B](f: A => B): MOption[B] = this match {
        case MNone => MNone
        case MSome(x) => MSome(f(x))
    }

    def flatMap[B](f: A => MOption[B]): MOption[B] = this match {
        case MNone => MNone
        case MSome(x) => f(x)
    }
    
    def getOrElse[B](default: => B): B = default
}
case class MSome[A](v: A) extends MOption[A]
case object MNone extends MOption[Nothing]

