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
class TechTreeTest  extends FunSuite {
  def slurpResource(str: String) =  {
    Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(str)).mkString
  }

  test("Elevators 0 tech tree") {
    val input = slurpResource("examples/pddl/tempo-sat/elevators/p00.pddl")
    val input2 = slurpResource("examples/pddl/tempo-sat/elevators/p00-domain.pddl")
    val problem = PDDL.parseProblem(input)
    val domain = PDDL.parseDomain(input2)

    val instance = ProblemInstance.fromPDDL(domain, problem)
    val init = instance.initialState

    val rel = instance.techTree.relevantActions(init, instance.goal)
    val moveSlowf0f1 = instance.groundedActionObj("move-up-slow", "slow0-0", "f0", "f1")

//    println(instance.techTree.actionsProvidingAxiom(instance.groundedAxiom("passenger-at", "p0", "f1")).map(instance.allViableGroundedActions))

    assert(rel.contains(moveSlowf0f1), rel)

  }

}
