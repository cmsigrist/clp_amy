object TupleCeption {
  def adder(v: (Int, Int)): (Int, (Int, Int)) = {
    v match {
        case (v1, v2) => 
            val res: Int = v1 + v2;
            (res, (v1, v2))
    }
  }
}
