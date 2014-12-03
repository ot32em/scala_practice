trait MTree[+A]
case class MLeaf[A](v: A) extends MTree[A]
case class MBranch[A](lt: MTree[A], rt: MTree[A]) extends MTree[A]

object MTree {
  def node_count[A](t: MTree[A]): Int = t match {
    case MBranch(left, right) => 1 + node_count(left) + node_count(right)
    case MLeaf(v) => 1
  }

  def maximum(t: MTree[Int]): Int = t match {
    case MBranch(left, right) => maximum(left) max maximum(right)
    case MLeaf(v) => v
  }

  def depth[A](t: MTree[A]): Int = t match {
    case MBranch(left, right) => 1 + (depth(left) max depth(right))
    case MLeaf(v) => 1
  }

  def sum(t: MTree[Int]): Int = t match {
    case MBranch(left, right) => sum(left) + sum(right)
    case MLeaf(v) => v
  }

  def map(t: MTree[Int])(f: Int => Int): MTree[Int] = t match {
    case MBranch(left, right) => MBranch(map(left)(f), map(right)(f))
    case MLeaf(v) => MLeaf(f(v))
  }
}

