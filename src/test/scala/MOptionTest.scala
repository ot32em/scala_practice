import org.scalatest._
import org.scalatest.Matchers._

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

}
    
