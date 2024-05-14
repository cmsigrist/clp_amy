object MatchFailDifferentTypesSrutinee {
  def a(): Int = {
    1 match {
      case true => 1
      case 2 => 1
      case "a" => 1
    }
  }
}
