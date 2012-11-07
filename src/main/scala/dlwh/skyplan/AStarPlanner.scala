package dlwh.skyplan

import dlwh.search.{SearchProblem, AStarSearch}
import collection.immutable.BitSet
import dlwh.skyplan.Expression.Global

/**
 * 
 * @author dlwh
 */
object AStarPlanner {
  def findPlan(inst: ProblemInstance) = {
    val goalAxioms = makeAxiomHeuristic(inst, inst.goal)

    def h(s: State) = 0.0

    def succ(s: State, cost: Double) = {
      s.relevantActions.map { case grounding =>
        val a = grounding.t
        val list = grounding.args
        val c = s.copy
        val grounded = s.problem.actions.ground(a, list)
        println(s, grounding)
        c.applyAction(grounded, a.durationOf(s, list))
        (c, grounding, c.cost - cost)
      }.toIndexedSeq
    }
    new AStarSearch[State, Grounded[IndexedAction]].search(SearchProblem(inst.initialState, succ _, {(s: State) => inst.goal.holds(s, s.makeContext())}, heuristic = h _))
  }

  def makeAxiomHeuristic(inst: ProblemInstance, cond: IndexedCondition):BitSet = cond match {
    case AndCondition(conds) => conds.map(makeAxiomHeuristic(inst, _)).reduce(_ ++ _)
    case PredicateCondition(i, args) =>
      if(args.forall(_.isInstanceOf[Global]))
        BitSet(inst.predicates.ground(i, args.map(_.asInstanceOf[Global].x)))
      else
        BitSet.empty
    case _ => BitSet.empty
  }


}
