import org.scalatest._
import org.scalatest.Matchers._
import MEither._

class MEitherTest extends FunSuite with Matchers
{
    test("exe 4.6 MEither ctor")
    {
        val a: MEither[String, Int] = MRight(1)
        a match {
            case MRight(v) => v shouldEqual 1
            case MLeft(v) => fail("Not right")
        }
    }


    def to_100(x: Double): Int = 100

    test("exe 4.6 map") {
        val v = MRight(1.0)
        val r = v.map(to_100)
        r shouldEqual MRight(100)
    }

    test("exe 4.6 map none case") {
        val v = MLeft("ERROR_VALUE")
        val r = v.map(to_100)
        r shouldEqual MLeft("ERROR_VALUE")
    }

    def to_100_if_pos(x: Double): MEither[String,Int] = 
        if(x > 0) MRight(100) else MLeft("ERROR_ARGUMENT")

    test("exe 4.6 flatMap") {
        val v = MRight(1.0)
        val r = v.flatMap(to_100_if_pos)
        r shouldEqual MRight(100)
    }
    test("exe 4.6 flatMap none case") {
        val v = MLeft("ERROR_VALUE")
        val r = v.flatMap(to_100_if_pos)
        r shouldEqual MLeft("ERROR_VALUE")
    }
    test("exe 4.6 flatMap invalid argument case") {
        val v = MRight(-2.0)
        val r = v.flatMap(to_100_if_pos)
        r shouldEqual MLeft("ERROR_ARGUMENT")
    }

    test("exe 4.6 orElse") {
        val v = MRight(10)
        val r = v.orElse(MRight(200))
        r shouldEqual MRight(10)
    }
    test("exe 4.6 orElse none case") {
        val v = MLeft("ERROR_VALUE")
        val r = v.orElse(MRight(200))
        r shouldEqual MRight(200)
    }

    def add(a: Int, b: Int): Int = a + b

    test("exe 4.6 map2") {
        val v = MRight(10)
        val v2 = MRight(200)
        val r = v.map2(v2)(add)
        r shouldEqual MRight(210)
    }

    test("exe 4.6 map2 none case") {
        val v = MRight(10)
        val err = MLeft("ERROR_VALUE")
        val r = v.map2(err)(add)
        r shouldEqual MLeft("ERROR_VALUE")

        val r2 = err.map2(v)(add)
        r2 shouldEqual MLeft("ERROR_VALUE")

        val r3 = err.map2(err)(add)
        r3 shouldEqual MLeft("ERROR_VALUE")
    }


    test("exe 4.7 sequence") {
        val as = List(MRight(1), MRight(2), MRight(3))
        val r = sequence(as)
        r shouldEqual MRight(List(1,2,3))
    }

    test("exe 4.7 sequence none case") {
        val as = List(MRight(1), MLeft("ERROR_VALUE"), MRight(3))
        val r = sequence(as)
        r shouldEqual MLeft("ERROR_VALUE")
    }


    test("exe 4.7 traverse") {
        val as = List(1.0, 2.0, 3.0)
        val r = traverse(as)(to_100_if_pos)
        r shouldEqual MRight(List(100,100,100))
    }

    test("exe 4.7 traverse invalid arg") {
        val as = List(1.0, -2.0, 3.0)
        val r = traverse(as)(to_100_if_pos)
        r shouldEqual MLeft("ERROR_ARGUMENT")
    }
}
