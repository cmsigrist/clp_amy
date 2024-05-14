object MatchFailDifferentTypesCaseClass {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  abstract class Tree
  case class Leaf(v: Int) extends Tree
  case class Node(l: Tree, r: Tree, v: Int) extends Tree

  def sum(l: List): Int = {
    l match {
      case Nil() => 0
      case Node(_, _, v) => v 
    }
  }

  
}
