package amyc.test

import amyc.parsing._
import amyc.ast.{Identifier, SymbolicPrinter}
import amyc.ast.SymbolicTreeModule.Program
import amyc.analyzer.{NameAnalyzer, TypeChecker}
import amyc.utils._
import org.junit.Test

class TyperTests extends TestSuite {
  // We need a unit pipeline
  private def unit[A]: Pipeline[A, Unit] = {
    new Pipeline[A, Unit] {
      def run(ctx: Context)(v: A) = ()
    }
  }

  val pipeline = Lexer andThen Parser andThen NameAnalyzer andThen TypeChecker andThen unit

  val baseDir = "typer"

  val outputExt = "" // No output files for typechecking
  
  @Test def testLetError1 = shouldFail("LetError1")
  @Test def testLetError2 = shouldFail("LetError2")

  @Test def testSeqError3 = shouldFail("SeqError3")

  @Test def testArithError1 = shouldFail("ArithError1")
  @Test def testArithmetic = shouldPass("Arithmetic")
  @Test def testOperators = shouldPass("Operators")
  @Test def testOperatorsError = shouldFail("OperatorsError")

  @Test def testMatch = shouldPass("Match")
  @Test def testMatchTuples = shouldPass("MatchTuples")
  @Test def testMatchFailDifferentType = shouldFail("MatchFailDifferentType")
  @Test def testMatchFailWildcard = shouldFail("MatchFailWildcard")
  @Test def testMatchFailConstructor = shouldFail("MatchFailConstructor")
  @Test def testMatchFailDifferentTypesScrutinee = shouldFail("MatchFailDifferentTypesScrutinee")
  @Test def testMatchFailDifferentTypesCaseClass = shouldFail("MatchFailDifferentTypesCaseClass")
  @Test def testMatchTupleAgainstClass = shouldFail("MatchTupleAgainstClass")
}
