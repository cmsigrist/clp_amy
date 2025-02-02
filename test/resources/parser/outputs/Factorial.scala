object Factorial {

  def fact(i: Int): Int = {
    (if((i < 2)) {
      1
    } else {
      (
        val rec: Int =
          fact((i - 1));
        (i * rec)
      )
    })
  }

  Std.printString(("5! = " ++ Std.intToString(fact(5))));
  Std.printString(("10! = " ++ Std.intToString(fact(10))))
}

