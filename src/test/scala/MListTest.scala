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

  // exe 3.5 
  // def dropWhile, which removes elements from the List prefix as long as they
  // match a predicate
  // def dropWhile[A](l: List[A], f: A=> Boolean): List[A]

  test("Exe_3.5 - dropWhile") {
    val a = MList(1,2,3,4,5,6)
    val r = dropWhile(a)((x) => x % 2 == 0)
    r shouldEqual MList(1,3,5)

    val r2 = dropWhile(a)((x) => x < 3 )
    r2 shouldEqual MList(3,4,5,6)
  }

  //
  // exe 3.5.2
  // def append[A], (List[A], List[A]) => List[A])
  test("Exe 3.5.2 append"){
    val a = MList(1,2,3)
    val b = MList(4,5,6)
    val r = append(a, b)
    r shouldEqual MList(1,2,3,4,5,6)

    val r2 = append(b, a)
    r2 shouldEqual MList(4,5,6,1,2,3)
  }

  test("Exe 3.5.2 append null case") {
    val a = MList(1,2,3)

    val r = append(NList, a)
    r shouldEqual a

    val r2 = append(a, NList)
    r2 shouldEqual a

    val r3 = append(NList, NList)
    r3 shouldEqual NList
  }

  // exe 3.6
  // init, that returns a List consisting of all but the last element of a List
  // def init[A] (List[A]) => List[A]
  test("Exe 3.6 init") {
    val a = MList(1,2,3,4)
    val r = init(a)
    r shouldEqual MList(1,2,3)

    val r2 = init(r)
    r2 shouldEqual MList(1,2)
  }

  test("exe 3.6 init null case") {
    val a = MList(1)
    val r = init(a)
    r shouldEqual NList

    val r2 = init(NList)
    r2 shouldEqual NList
  }

  test("exe 3.9 length") {
    val a = MList(1,2,3,4,5)
    val r = Length(a)
    r shouldEqual 5

    val a2 = MList(1,2,3)
    val r2 = Length(a2)
    r2 shouldEqual 3
  }

  test("exe 3.9 length null case") {
    val a = NList
    val r = Length(a)
    r shouldEqual 0
  }

  test("exe 3.12 reverse") {
    val a = MList(1,2,3,4)
    val r = reverse(a)
    r shouldEqual MList(4,3,2,1)
  }
  
  test("exe foldRight") {
    val a = MList(1,2,3)
    val r = foldRight(a, 0)(_+_)
    r shouldEqual 6
  }

  test("exe 3.15 flat list") {
    val a = MList(MList(1,2,3), MList(4,5,6), MList(7,8,9))
    val b = flat(a)
    b shouldEqual MList(1,2,3,4,5,6,7,8,9)
  }

  test("exe 3.16 plus1") {
    val a = MList(1,2,3)
    val r = plus1(a)
    r shouldEqual MList(2,3,4)
  }

  test("exe 3.17 turnString") {
    val a = MList[Double](1.1, 2.2, 3.3)
    val r = turnString(a)
    r shouldEqual MList("1.1", "2.2", "3.3")
  }

  test("exe 3.18 map") {
    val a = MList(1,2,3)
    val r = map(a)(_+1)
    r shouldEqual MList(2,3,4)

    val r2 = map(a)(_*2)
    r2 shouldEqual MList(2,4,6)
  }

  test("exe 3.18 map string -> size") {
    val a = MList("Hello", "world!!", "It's", "me.")
    val r = map(a)(_.size)
    r shouldEqual MList(5, 7, 4, 3)
  }

  test("exe 3.19 filter") {
    val a = MList(1,2,3,4,5)
    val r = filter(a)(_%2 == 0)
    r shouldEqual MList(2,4)

    val r2 = filter(a)(_%2 == 1)
    r2 shouldEqual MList(1,3,5)
  }

  test("exe 3.20 flatMap") {
    val a = MList(1,2,3)
    val r = flatMap(a)(i=>MList(i,i))
    r shouldEqual MList(1,1,2,2,3,3)
  }

  test("exe 3.22 zipWith") {
    val a = MList(1,2,3)
    val b = MList(7,6,5)
    val r = zipWith(a, b)(_+_)
    r shouldEqual MList(8,8,8)
  }

  test("exe 3.24 hasSubsequence") {
    val a = MList(1,2,3,4)

    val p1 = MList(1,2)
    assert( hasSubseq(a, p1) )

    val p2 = MList(2,3,4)
    assert( hasSubseq(a, p2))

    val p3 = MList(3,4)
    assert( hasSubseq(a, p3))
  }

  test("exe 3.24 hasSubseq non-exists cases"){
    val a = MList(1,2,3,4)

    val n1 = MList(1,3)
    assert( !hasSubseq(a, n1) )
    
    val n2 = MList(1,4)
    assert( !hasSubseq(a, n2) )

    val n3 = MList(1,3,4)
    assert( !hasSubseq(a, n3) )

    val n4 = MList(5,1,2,3,4)
    assert( !hasSubseq(a, n4) )
  }
    
    
}
