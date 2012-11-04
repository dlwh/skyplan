package dlwh.skyplan

import org.scalatest.FunSuite
import io.Source

/**
 * 
 * @author dlwh
 */
class DeleteFreeTest extends FunSuite {
  def slurpResource(str: String) =  {
    Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(str)).mkString
  }

  test("ensure delete-free-ness for resources") {
    val input = slurpResource("examples/pddl/settlers/domain.pddl")
    val input2 = slurpResource("examples/pddl/settlers/pfile1")
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)


    val instance = ProblemInstance.fromPDDL(domain, problem).deleteFreeVersion
    val init: State = instance.initialState
    init.applyAction(instance.groundedAction("build-cabin", "location0"), 0)
    init.applyAction(instance.groundedAction("fell-timber", "location0"), 0)
    assert(init.resource("available","timber","location0") === 1)
    init.applyAction(instance.groundedAction("build-cart", "location0", "vehicle0"), 0)
    assert(init.resource("available", "timber","location0") === 1)
    assert(init.hasAxiom("potential", "vehicle0"))

  }

}
