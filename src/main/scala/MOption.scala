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

    def filter(f: A => Boolean): MOption[A] = this match {
        case MSome(v) => if (f(v)) this else MNone
        case _ => MNone
    }
}
case class MSome[A](v: A) extends MOption[A]
case object MNone extends MOption[Nothing]


object MOption
{
    def variance(as: Seq[Double]): MOption[Double] = {
        def mean(as: Seq[Double]): Double = {
            as.foldLeft(0.0)(_+_) / as.size
        }
        if(as.isEmpty) MNone
        else MSome(mean(as.map(v => math.pow(v - mean(as), 2))))
    }

}
