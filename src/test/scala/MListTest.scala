import org.scalatest.FunSuite
import mlist._

class MListTest extends FunSuite
{

  test("Construct MList")
  {
    val l = MCons(1, MCons(2, NList))
    assert(l.head == 1)
    assert(l.tail == MCons(2, NList))
  }

  test("Construct MList via List Literal")
  {
    val l = MList(1,2,3)
    val v = l match {
      case MCons(h, t) => {assert( h == 1); h}
      case NList => { assert(false); 0 }
    }
    assert(v == 1)
  }

  test("Test sum for add ints in MList")
  {
    val ints = MList(1,2,10)
    assert(MList.sum(ints) == 1+2+10)

  }
}
