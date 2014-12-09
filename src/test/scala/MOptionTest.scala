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

  test("Option map non none")
  {
      val a:MOption[Double] = MSome(1.1)
      val b = a.map(x => 100)
      b shouldEqual 100
  }

  test("Option map none case")
  {
      val a:MOption[Double] = MNone
      val b = a.map(x => 100)
      b shouldEqual MNone
  }
}
    
