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
    
    def getOrElse[B >: A](default: => B): B = this match {
        case MNone => default
        case MSome(v) => v
    }

    def orElse[B >: A](default: => MOption[B]): MOption[B] = this match {
        case MNone => default
        case _ => this
    }

    def filter(f: A => Boolean): MOption[A] = this
}
case class MSome[A](v: A) extends MOption[A]
case object MNone extends MOption[Nothing]

