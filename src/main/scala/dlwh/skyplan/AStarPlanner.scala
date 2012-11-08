package dlwh.skyplan

import dlwh.search.{SearchProblem, AStarSearch}
import dlwh.skyplan.Expression.Global

/**
 * 
 * @author dlwh
 */
object AStarPlanner {
  def findPlan(inst: ProblemInstance) = {
    def h(s: State) = 0.0

    def succ(s: State, cost: Double): IndexedSeq[(State, Option[Grounded[IndexedAction]], Double)] = {
      val do_actions = s.relevantActions.map { case grounding =>
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
    new AStarSearch[State, Option[Grounded[IndexedAction]]].search(SearchProblem(inst.initialState, succ _, {(s: State) => inst.goal.holds(s, s.makeContext())}, heuristic = h _))
  }



}
