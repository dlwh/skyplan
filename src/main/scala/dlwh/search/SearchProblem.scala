package dlwh.search

/**
 * 
 * @author dlwh
 */
trait SearchProblem[State, Action] {

  def init: State
  /**
   *
   * @param state
   * @param currentCost
   * @return newState, action/operator to get to that state, cost delta.
   */
  def successors(state: State, currentCost: Double):IndexedSeq[(State, Action, Double)]

  def isGoal(state: State): Boolean

  def heuristic(state: State): Double = 0.0
}

object SearchProblem {
  def apply[State, Action](init: State,
                           successors: (State, Double)=>IndexedSeq[(State,Action,Double)],
                           isGoal: State=>Boolean,
                           heuristic: State=>Double = {(_: State) => 0.0}):SearchProblem[State, Action] = {
    val i = init
    val s = successors
    val ig = isGoal
    val h = heuristic
    new SearchProblem[State, Action] {
      def init: State = i
      def successors(state: State, currentCost: Double) = s(state, currentCost)
      def isGoal(state: State): Boolean = ig(state)
      override def heuristic(state: State): Double = h(state)
    }
  }
}
