object OperatorsAreShortCircuiting {
  
  def shouldNotEnter(): Boolean = {
    Std.printString("This should not be printed");
    true
  }

  if (false && shouldNotEnter()) {
    Std.printString("Smthg is terribly wrong")
  } 
  else {
    if (true || shouldNotEnter()) {
      Std.printString("OK")
    }
    else {
      Std.printString("Smthg is terribly wrong")
    }
  }
}
