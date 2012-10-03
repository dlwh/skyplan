package dlwh.skyplan

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import io.Source

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class ProblemInstanceTest  extends FunSuite {
  def slurpResource(str: String) =  {
    Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(str)).mkString
  }

  test("Strips Rover") {
    val input = slurpResource("examples/rover/strips/problem.pddl")
    val input2 = slurpResource("examples/rover/strips/domain.pddl")
    val problem = PDDL.parseProblem(input)
    val domain = PDDL.parseDomain(input2)

    val instance = ProblemInstance.fromPDDL(domain, problem)
    instance.initialState
    println(instance.initialState.toString)
    println(instance.initialState.possibleActions)
  }

}
