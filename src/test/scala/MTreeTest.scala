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


  def fixture(): MTree[Int] = {
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
    MBranch(ab, c)
  }

  test("exe 3.25 node_count") {
    val f = fixture()
    val r = node_count(f)
    r shouldEqual 11
  }

  test("exe 3.26 maximum") {
    val f = fixture()
    val r = maximum(f)
    r shouldEqual 6
  }

  test("exe 3.27 depth") {
    val f = fixture()
    val r = depth(f)
    r shouldEqual 4
  }
}


