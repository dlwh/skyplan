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
class StateTest  extends FunSuite {
  def slurpResource(str: String) =  {
    Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(str)).mkString
  }

  test("equals and hashcode work with copy and successors") {
    val input = slurpResource("examples/pddl/woodworking/p01-domain.pddl")
    val input2 = slurpResource("examples/pddl/woodworking/p01.pddl")
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)
    val instance = ProblemInstance.fromPDDL(domain, problem)
    var state: State = instance.initialState



    for(i <- 0 until 10) {
      val copy = state.copy
      assert(state.resources === copy.resources)
      assert(state.axioms === copy.axioms)
      assert(state.cost === copy.cost)
      assert(state.pendingActions.data === copy.pendingActions.data)
      assert(state === copy)
      val actions = copy.relevantActions.toIndexedSeq
      if(actions.length > 0) {
        val grounding = actions(math.random * actions.length toInt)
        val a = grounding.t
        val list = grounding.args
        val grounded = state.problem.actions.ground(a, list)
        copy.applyAction(grounded, a.durationOf(copy, list))
        copy.elapseTime()
      }
      state = copy
    }
  }

}
