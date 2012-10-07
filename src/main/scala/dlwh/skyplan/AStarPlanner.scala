package dlwh.skyplan

import dlwh.search.AStarSearch

/**
 * 
 * @author dlwh
 */
object AStarPlanner {
  def findPlan(inst: ProblemInstance) = {
    def succ(s: State) = {
      println(s)
      s.possibleActions.map { a =>
        val c = s.copy
        c.applyAction(a)

        (c, a, 0.0)
      }
    }
    AStarSearch.search(inst.initialState, succ _, {(s: State) => inst.goal.holds(s, s.makeContext())})
  }

}
