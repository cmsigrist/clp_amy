object Match {
  3 + 2 match {
    case 5 => Std.printString("OK");Std.printInt(-5)
    case _ => 
      Std.printString("Not OK");
      val message: String = "+ should precede match";
      Std.printString(message);
      Std.printString(message)
  }
}
