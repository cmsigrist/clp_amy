package amyc.test

import amyc.parsing._
import org.junit.Test

class LexerTests extends TestSuite {
  val pipeline = Lexer andThen DisplayTokens

  val baseDir = "lexer"

  val outputExt = "txt"

  @Test def testKeywords = shouldOutput("Keywords")

  @Test def testSingleAmp = shouldFail("SingleAmp")
  
  @Test def testComments = shouldOutput("Comments")
  @Test def testCommentClosedTwice = shouldOutput("CommentClosedTwice")
  @Test def testUnclosedComment3 = shouldFail("UnclosedComment3")

  @Test def testWhitespace = shouldOutput("Whitespace")

  @Test def testOperators = shouldOutput("Operators")
  // Test that integers bounds are coherent with the ref implementation
  @Test def testIntegersBoundsUp = shouldFail("IntegersOutOfBoundsUp")
  @Test def testIntegersBoundsDown = shouldFail("IntegersOutOfBoundsDown")
  @Test def testIntegers = shouldOutput("Integers")
}
