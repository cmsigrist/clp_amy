object MatchFailDifferentTypes {
  def a(): Int = {
    1 match {
      case 1 => 1
      case 2 => "a"
      case 3 => true
      case 4 => ()
    }
  }
}
