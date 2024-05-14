object BinOps {
  def a(): Unit = {
    false || error(":-/");
    a() && true;
    false + a()
  }
}
