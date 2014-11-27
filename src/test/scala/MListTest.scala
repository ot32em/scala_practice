import org.scalatest.FunSuite
import mlist._

class MListTest extends FunSuite
{

  test("exe_3 Construct MList")
  {
    val l = MCons(1, MCons(2, NList))
    assert(l.head == 1)
    assert(l.tail == MCons(2, NList))
  }

  test("exe_3 Construct MList via List Literal")
  {
    val l = MList(1,2,3)
    val v = l match {
      case MCons(h, t) => {assert( h == 1); h}
      case NList => { assert(false); 0 }
    }
    assert(v == 1)
  }

  test("exe_3 Test sum for add ints in MList") {
    val ints = MList(1,2,10)
    assert(MList.sum(ints) == 1+2+10)

    val ints2 = MList(2, -2)
    assert(MList.sum(ints2) == 0)

    assert(MList.sum(NList) == 0)
    
  }

  test("exe_3.2 - tail") {
    val ints = MList(1,2,3,4,5)
    val ints2 = MList.tail(ints)
    assert(ints2 == MList(2,3,4,5))

    val ints3 = MList.tail(MList.tail(ints2))
    assert(ints3 == MList(4,5))

    val one = MList(3)
    assert(MList.tail(one) == NList)

  }
  
  test("exe_3.2 - tail null case") {
    assert(MList.tail(NList) == NList)
  }

}
