trait MTree[+A]
case class MLeaf[A](v: A) extends MTree[A]
case class MBranch[A](lt: MTree[A], rt: MTree[A]) extends MTree[A]

object MTree {
  def node_count[A](t: MTree[A]): Int = 0
}

