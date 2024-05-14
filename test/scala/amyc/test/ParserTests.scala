package amyc.test

import amyc.parsing._
import org.junit.Test

class ParserTests extends TestSuite with amyc.MainHelpers {
  val pipeline = Lexer andThen Parser andThen treePrinterN("")

  val baseDir = "parser"

  val outputExt = "scala"

  @Test def testLL1 = {
    assert(Parser.program.isLL1)
  }

  @Test def testEmpty = shouldOutput("Empty")
  @Test def testLiterals = shouldOutput("Literals")
  @Test def testVal = shouldOutput("Val")
  @Test def testLetMatch = shouldOutput("LetMatch")
  @Test def testChainedMatch = shouldOutput("ChainedMatch")
  @Test def testBinOps = shouldOutput("BinOps")

  @Test def testArithmetic = shouldOutput("Arithmetic")
  @Test def testFactorial = shouldOutput("Factorial")
  @Test def testHanoi = shouldOutput("Hanoi")
  @Test def testHelloInt= shouldOutput("HelloInt")
  @Test def testHello = shouldOutput("Hello")
  @Test def testMatch = shouldOutput("Match")
  @Test def testPrinting = shouldOutput("Printing")
  @Test def testTestLists = shouldOutput("TestLists")
  
  @Test def testEmptyFile = shouldFail("EmptyFile")
  @Test def testNestedVal = shouldFail("NestedVal")
  @Test def testDoubleUnary = shouldFail("DoubleUnary")
}

