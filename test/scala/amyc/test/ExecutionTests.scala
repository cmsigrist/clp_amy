package amyc.test

import org.junit.Test

abstract class ExecutionTests extends TestSuite {

  val baseDir = "interpreter"

  val outputExt = "txt"

  @Test def testEmptyObject = shouldOutput("EmptyObject")
  @Test def testTupleCeption = shouldOutput(
    List("../../../../library/Std", "TupleCeption"), "TupleCeption"
  @Test def testOperatorsAreShortCircuiting = shouldOutput(
    List("../../../../library/Std", "OperatorsAreShortCircuiting"),
    "OperatorsAreShortCircuiting"
  )

  @Test def testMinimalError = shouldFail("MinimalError")

}
