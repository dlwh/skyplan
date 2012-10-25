package dlwh.skyplan

import dlwh.search.AStarSearch
import collection.immutable.BitSet
import dlwh.skyplan.Expression.Global

/**
 * 
 * @author dlwh
 */
object AStarPlanner {
  def findPlan(inst: ProblemInstance) = {
    val goalAxioms = makeAxiomHeuristic(inst, inst.goal)

    println(goalAxioms.map(inst.predicates.groundedIndex.get _))


    def h(s: State) = 0.0

    def succ(s: State, cost: Double) = {
      s.relevantActions.map { case grounding =>
        val a = grounding.t
        val list = grounding.args
        val c = s.copy
        val grounded = s.problem.actions.ground(a, list)
        println(s, grounding)
        c.applyAction(grounded, a.durationOf(s, list))
        (c, a, c.cost - cost)
      }.toIndexedSeq
    }
    new AStarSearch[State].search(inst.initialState, succ _, {(s: State) => inst.goal.holds(s, s.makeContext())}, h = h _)
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
