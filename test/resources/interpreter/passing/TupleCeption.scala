object TupleCeption {
  abstract class TupVal
  case class Nil() extends TupVal
  case class Cons(v: (Int, TupVal)) extends TupVal

  def loop(a: TupVal): Unit = {
    a match {
      case Cons((x, xs)) => 
        Std.printInt(x);
        loop(xs)
      case Nil() => Std.printString("End of list")
    }
  }

  val a: TupVal = Cons((1, Cons((2, Cons((3, Nil()))))));
  loop(a)
}
