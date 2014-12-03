trait MTree[+A]
case class MLeaf[A](v: A) extends MTree[A]
case class MBranch[A](lt: MTree[A], rt: MTree[A]) extends MTree[A]

object MTree {
  def node_count[A](t: MTree[A]): Int = t match {
    case MBranch(left, right) => 1 + node_count(left) + node_count(right)
    case MLeaf(v) => 1
  }

  def maximum(t: MTree[Int]): Int = 0

}

