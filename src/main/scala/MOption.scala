sealed trait MOption[+A]
{
    def map[B](f: A => B): MOption[B] = MNone
}
case class MSome[A](v: A) extends MOption[A]
case object MNone extends MOption[Nothing]

