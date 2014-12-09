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

}
    
