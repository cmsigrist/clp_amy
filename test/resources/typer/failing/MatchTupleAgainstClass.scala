object MatchTupleAgainstClass{
  abstract class Tuple
  case class Tuple2(first: Int, second: Boolean) extends Tuple

  def maybeNeg(v: (Int, Boolean)): (Int, Boolean) = {// Type
    v match {
      case Tuple2(i, false) =>// pattern
        (i, false)// literal
      case (i, true) =>
        (-i, false)
    }
  }
}
