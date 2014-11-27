import mlist._

object main extends App{
  val x = MList(1,2,3,4,5) match {
    case MCons(x, MCons(2, MCons(4, _))) => x
    case NList => 42
    case MCons(x, MCons(y, MCons(3, MCons(4, _)))) => x + y
    case MCons(h, t) => h + MList.sum(t)
    case _ => 101
  }
  println(x)
  println("Hello")
}
