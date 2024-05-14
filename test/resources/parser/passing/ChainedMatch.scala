object ChainedMatch {
  def mat(x: Int): Int = {
    (x match {
      case 1 => 2 match {
        case 1 => 1
        case 2 => 2
      }
      case 2 => 2
    }) match {
      case 1 => 0
      case 2 => 2
    }
  }
}
