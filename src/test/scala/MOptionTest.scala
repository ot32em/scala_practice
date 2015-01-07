import org.scalatest._
import org.scalatest.Matchers._
import MOption._

class MOptionTree extends FunSuite with Matchers
{
  test("exe 4.0 Options ctor")
  {
    val a: MOption[Int] = MSome(1)
    val r = a match {
      case MSome(v) => v
      case MNone => 0
    } 
    r shouldEqual 1 
  }

  def to_100(x: Double): Int = 100

  test("exe 4.1 Option map non none")
  {
      val a = MSome(1.1)
      val b = a.map(to_100)
      b shouldEqual MSome(100)
  }

  test("exe 4.1 Option map none case")
  {
      val a = MNone
      val b = a.map(to_100)
      b shouldEqual MNone
  }

  def to_o100(x: Double): MOption[Int] = MSome(100)

  test("exe 4.1 Option flatMap some")
  {
      val a = MSome(1.1)
      val b = a.flatMap(to_o100)
      b shouldEqual MSome(100)
  }

  test("exe 4.1 Option flatMap none")
  {
      val a = MNone
      val b = a.flatMap(to_o100)
      b shouldEqual MNone
  }

  test("exe 4.1 Option getOrElse nonnone")
  {
      val a = MSome(1.1)
      val b = a.getOrElse(9.9)
      b shouldEqual (1.1 plusOrMinus 0.1)
  }
  test("exe 4.1 Option getOrElse none")
  {
      val a = MNone
      val b = a.getOrElse(9.9)
      b should be (9.9 plusOrMinus 0.1)
  }

  test("exe 4.1 Option orElse nonnone")
  {
      val a = MSome(1.1)
      val b = a.orElse(MSome(9.9))
      val c = b match {
          case MSome(v) => v
          case _ => 0.0
      }
      c should be (1.1 plusOrMinus 0.1)
  }

  test("Exe 4.1 Option orElse none")
  {
      val a = MNone
      val b = a.orElse(MSome(9.9))
      val c = b match {
          case MSome(v) => v;
          case _ => 0.0
      } 
      c should be (9.9 plusOrMinus 0.1)
  }

  test("Exe 4.1 Option filter non none")
  {
      val a = MSome(5)
      val b = a.filter( _ % 5 == 0)
      b shouldEqual MSome(5)
  }

  test("Exe 4.1 Option filter none")
  {
      val a: MOption[Int] = MSome(4)
      val b = a.filter( _ % 5 == 0)
      b shouldEqual MNone
  }

  test("Exe 4.2 variance in terms of option::flatMap")
  {
      val a = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
      val b = MOption.variance(a) match {
          case MSome(v) => v
          case _ => 0.0
      }
      b shouldEqual (2.0 plusOrMinus 0.1)
  }

  test("Exe 4.2 variance in terms of option::flatMap empty Seq")
  {
      val a = Seq()
      val b = MOption.variance(a) 
      b shouldEqual MNone
  }

  def divide(a: Int, d: Int): Int = a / d

  test("Cht 4 theTry non-throw")
  {
      val v = divide(10,2)
      v shouldEqual 5

      try
      {
        val v = theTry(divide(10,5))
        v shouldEqual MSome(2)
      }
      catch
      {
        case _: Throwable => fail("theTry(divide) should not throw exception")
      }
  }

  test("Cht 4 theTry throw case")
  {
      intercept[java.lang.ArithmeticException]{
          divide(1,0)
      }
      try
      {
        val v = theTry(divide(1,0))
        v shouldEqual MNone
      }
      catch
      {
        case _: Throwable => fail("theTry(divide) should not throw exception")
      }
  }

  def add1(a: Int): Int = a + 1

  test("Cht 4 lift")
  {
      val v = lift(add1)(MSome(1))
      v shouldEqual MSome(2)
  }

  test("Cht 4 lift none case ")
  {
      val v = lift(add1)(MNone)
      v shouldEqual MNone
  }

  def add(a: Int, b: Int): Int = a + b

  test("ex 4.3 map2 valid case")
  {
     val v = map2(MSome(1), MSome(1))(add)
     v shouldEqual MSome(2)
  }

  test("exe 4.3 map2 none case")
  {
      val v = map2(MSome(1), MNone)(add)
      v shouldEqual MNone

      val v2 = map2(MNone, MSome(1))(add)
      v2 shouldEqual MNone
  }

  val as = List(MSome(1), MSome(2), MSome(3))
  test("exe 4.4 traverse all ok ")
  {
      val v = traverse(as)
      v shouldEqual MSome(List(1,2,3))
  }

  test("exe 4.4 traverse met one none")
  {
      val bs = as :+ MNone
      val v = traverse(bs)
      v shouldEqual MNone
  }
}
