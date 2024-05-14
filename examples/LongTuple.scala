object LongTuple {
  def adder(v: (Int, Int, Int, Int, Int, Int, Int, Int)): Int = {
    v match {
        case (v1, v2, v3, v4, v5, v6, v7, v8) => 
            v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8
    }
  }
}
