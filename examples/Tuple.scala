object Tuple {
  def maybeNeg(v: (Int, Boolean)): (Int, Boolean) = {// Type
    v match {
      case (i, false) =>// pattern
        (i, false)// literal
      case (i, true) =>
        (-i, false)
    }
  }

  val a: (Int, Boolean) = (1, true);
  maybeNeg(a) match {
    case (i, j) => Std.printInt(i)
  }
}
