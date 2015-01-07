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
    def mean(as: Seq[Double]): MOption[Double] = {
        if(as.isEmpty) MNone
        else MSome(as.foldLeft(0.0)(_+_) / as.size)
    }

    def variance(as: Seq[Double]): MOption[Double] = {
        mean(as) flatMap (m => mean(as.map(a => math.pow(a-m, 2))))
    }


    //m = n1 + n2 + ... + nn / n
    //v = (n1 - m)^2 + (n2 - m)^2 + ... + (nn - m)^2 / n
    //

    def theTry[A](a: => A): MOption[A] = {
        try MSome(a)
        catch { case _: Throwable => MNone }
    }

    def lift[A, B](f: A => B): MOption[A] => MOption[B] = (x: MOption[A]) => MNone
}
