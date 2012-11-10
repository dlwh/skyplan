package dlwh.skyplan

import dlwh.search.{StateSkyline, NaiveSkyline, HierarchicalSkylineSearch, SearchProblem}
import io.Source

/**
 * 
 * @author dlwh
 */
object HierarchicalSkyplan {
  def findPlan(inst: ProblemInstance) = {
    implicit val ordering = Skyplan.makeOrdering(inst)
    def succ(s: State, cost: Double, goal: IndexedCondition): IndexedSeq[(State, Option[Grounded[IndexedAction]], Double)] = {
      val do_actions = s.relevantActions(goal).map { case grounding =>
        val a = grounding.t
        val list = grounding.args
        val c = s.copy
        val grounded = s.problem.actions.ground(a, list)
        c.applyAction(grounded, a.durationOf(s, list))
        (c, Some(grounding), c.cost - cost)
      }.toIndexedSeq

      if(s.hasAction()) do_actions :+ { val s2 = s.copy; s2.elapseTime(); (s2, None, s2.cost - cost)}
      else do_actions
    }

    val projected = projectionHierarchy(inst).map{ inst =>
      SearchProblem(inst.initialState, {succ(_:State, _: Double, inst.goal)}, {(s: State) => inst.goal.holds(s, s.makeContext())})
    }

    val search = new HierarchicalSkylineSearch[State, Option[Grounded[IndexedAction]]](StateSkyline.factory(inst))
    search.search(projected, IndexedSeq.fill(projected.length - 1)(identity))
  }

    def projectionHierarchy(inst: ProblemInstance):IndexedSeq[ProblemInstance] = inst.goal match {
      case AndCondition(conds) if conds.length > 1 => IndexedSeq(inst, inst.copy(goal=conds.head))
      case _ =>  IndexedSeq(inst)
    }


  def main(args: Array[String]) {
    def slurpResource(str: String) =  {
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(str)).mkString
    }
    //    val input = slurpResource("examples/pddl/settlers/domain.pddl")
    //    val input2 = slurpResource("examples/pddl/settlers/pfile0")
        val input = slurpResource("examples/pddl/woodworking/p03-domain.pddl")
        val input2 = slurpResource("examples/pddl/woodworking/p03.pddl")
//    val input = slurpResource("examples/pddl/openstacks/p01-domain.pddl")
//    val input2 = slurpResource("examples/pddl/openstacks/p01.pddl")
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)


    try {

      val instance = ProblemInstance.fromPDDL(domain, problem)
      val plan = HierarchicalSkyplan.findPlan(instance)
      assert(plan.nonEmpty,plan)
      println(plan)
    } catch {
      case e =>
        e.printStackTrace()
        throw e
    }
  }
}
