trait MTree[+A]
case class MLeaf[A](v: A) extends MTree[A]
case class MBranch[A](lt: MTree[A], rt: MTree[A]) extends MTree[A]

object MTree {
  def node_count[A](t: MTree[A]): Int = fold(t,1)(1+_+_)((_,x)=>x) 

  def fold[A, B](t: MTree[A], z: B)(f: (B,B)=>B)(g: (A,B)=>B): B = t match {
    case MBranch(left, right) => f(fold(left,z)(f)(g), fold(right,z)(f)(g))
    case MLeaf(v) => g(v,z)
  }

  def maximum(t: MTree[Int]): Int = fold(t,0)(_ max _)((v,_)=>v)

  def depth[A](t: MTree[A]): Int = fold(t,1)((l,r)=> 1 + l max r)((_,x)=>x)

  def sum(t: MTree[Int]): Int = fold(t,0)(_+_)((x,_)=>x)

  def map(t: MTree[Int])(f: Int => Int): MTree[Int] = t match {
    case MBranch(left, right) => MBranch(map(left)(f), map(right)(f))
    case MLeaf(v) => MLeaf(f(v))
  }
}

