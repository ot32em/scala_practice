trait MTree[+A]
case class MLeaf[A](v: A) extends MTree[A]
case class MBranch[A](lt: MTree[A], rt: MTree[A]) extends MTree[A]

object MTree {
  def node_count[A](t: MTree[A]): Int = fold(t)(a=>1)(1+_+_) 

  def maximum(t: MTree[Int]): Int = fold(t)(v=>v)(_ max _)

  def depth[A](t: MTree[A]): Int = fold(t)(_=>1)(1+_ max _)

  def sum(t: MTree[Int]): Int = fold(t)(x=>x)(_+_)

  def map(t: MTree[Int])(f: Int => Int): MTree[Int] = 
    fold(t)(v=>MLeaf(f(v)): MTree[Int])(MBranch(_,_))

  def fold[A, B](t: MTree[A])(g: (A)=>B)(f: (B,B)=>B): B = t match {
    case MBranch(left, right) => f(fold(left)(g)(f), fold(right)(g)(f))
    case MLeaf(v) => g(v)
  }
}

