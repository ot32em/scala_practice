import org.scalatest.FunSuite
import org.scalatest.Matchers
import mlist._
import mlist.MList._

class MListTest extends FunSuite with Matchers
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
    assert(sum(ints) == 1+2+10)

    val ints2 = MList(2, -2)
    assert(sum(ints2) == 0)

    assert(sum(NList) == 0)
    
  }

  test("exe_3 - product") {
    val a = MList(1.1, 2.2, 3.3)
    assert(product(a) == 1.1 * 2.2 * 3.3)

    val result2 = product(MList(1.1, -5.5, 0.9))
    val expected2 = 1.1 * -5.5 * 0.9
    result2 should be (expected2 plusOrMinus 0.00001)
  }

  test("exe_3 - product 0 case") {
    val a1 = MList(1.1, 0.0, 2.2, 3.3)
    assert(product(a1) == 0.0)
    val a2 = MList(-1.1, -4321.2, 0.0, 2.2, 3.3)
    assert( product(a2) == 0.0 )
  }

  test("exe_3 - product null case") {
    assert(product(NList) == 1.0)
  }
  test("exe_3.2 - tail") {
    val ints = MList(1,2,3,4,5)
    val ints2 = tail(ints)
    assert(ints2 == MList(2,3,4,5))

    val ints3 = tail(tail(ints2))
    assert(ints3 == MList(4,5))

    val one = MList(3)
    assert(tail(one) == NList)

  }
  
  test("exe_3.2 - tail null case") {
    assert(tail(NList) == NList)
  }

  test("exe_3.3 - setHead") {
    val a = MList(1,2,3,4,5)
    val r = setHead(a, 999)
    r should equal (MList(999,2,3,4,5))

    val a2 = MList(6,31,21,3)
    val r2 = setHead(a2, -3)
    r2 should equal (MList(-3,31,21,3))
  }

  test("exe_3.3 - setHead null case") {
    val r = setHead(NList, 3)
    r should equal (NList)
  }


  test("exe_3.4 - drop") {
    val a = MList(1,2,3,4,5)
    drop(a, 2) shouldEqual MList(3,4,5)
    drop(a, 4) shouldEqual MList(5)
    drop(a, 5) shouldEqual NList
  }

  test("Exe_3.4 - drop too much") {
    val a = MList(1,2,3,4,5)
    drop(a, 6) shouldEqual NList
    drop(a, 19) shouldEqual NList
  }

  test("Exe_3.4 - drop negative items") {
    val a = MList(1,2,3,4,5)
    drop(a, -1) shouldEqual a
    drop(a, -10) shouldEqual a
  }

  test("Exe_3.4 - drop null case") {
    drop(NList, 1) shouldEqual NList
  }
}
