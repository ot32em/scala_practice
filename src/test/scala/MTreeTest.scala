import org.scalatest.FunSuite
import org.scalatest.Matchers
import MTree._

class MTreeTest extends FunSuite with Matchers
{
  test("Tree Structure") {
    val a: MTree[Int] = MBranch(MLeaf(1), MLeaf(2))
    val r = a match {
      case MBranch(l,r) => (l, r)
      case _ => (0,0)
    }
    r._1 shouldEqual MLeaf(1)
    r._2 shouldEqual MLeaf(2)
  }

  test("exe 3.25 node_count") {
    /**
     *              x
     *        x           x
     *    x       x     5    6
     *  1   2   3   4
     *
     */
    val a = MBranch(MLeaf(1), MLeaf(2))
    val b = MBranch(MLeaf(3), MLeaf(4))
    val c = MBranch(MLeaf(5), MLeaf(6))
    val ab = MBranch(a, b)
    val ab_c = MBranch(ab, c)
    val r = node_count(ab_c)
    r shouldEqual 11
  }
}


