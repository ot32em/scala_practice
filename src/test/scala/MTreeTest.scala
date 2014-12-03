import org.scalatest.FunSuite
import org.scalatest.Matchers

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
}


