object Match {

  (3 + 2) match {
    case 5 =>
      (
        Std.println("OK");
        Std.println(-(5))
      )
    case _ =>
      (
        Std.println("Not OK");
        val message: String =
          "+ should precede match";
        Std.println(message);
        Std.println(message)
      )
  }
}

