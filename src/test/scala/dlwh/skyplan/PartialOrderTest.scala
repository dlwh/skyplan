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
class PartialOrderTest  extends FunSuite {
  def slurpResource(str: String) =  {
    Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(str)).mkString
  }

  test("Settlers 1") {
    val input = slurpResource("examples/pddl/settlers/domain.pddl")
    val input2 = slurpResource("examples/pddl/settlers/pfile1")
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)


    try {

      val instance = ProblemInstance.fromPDDL(domain, problem)
      val init: State = instance.initialState
      val compare = init.copy
      val resource = instance.valFuns.index("pollution")
      compare.resources(resource) += 2

      val checker = new DominanceChecker(instance)
      val cmp = checker.compareStates(init, compare)

      assert(cmp === Equals)
    } catch {
      case e =>
        e.printStackTrace()
      throw e
    }
  }
}

