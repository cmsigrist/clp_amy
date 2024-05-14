object Match {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  def doA(): Int = {
    val l: List = Cons(1, Cons(2, Cons(3, Nil())));
    sum(l)
  }

  def sum(l: List): Int = {
    l match {
      case Cons(x, xs) => x + sum(xs)
      case Nil() => 0
    }
  }

  3 + 2 match {
    case 5 => 6
    case _ => 1
  };
  2 match {
    case 1 => 1
    case a => a
  }
  
}
