package dlwh.skyplan


case class GroundedAction(ungrounded: IndexedAction,
                          argAssignments: IndexedSeq[Int],
                          completionTime: Double) {
  def canExecute(state: State) = {
    ungrounded.precondition.forall(_.holds(state, state.makeContext(argAssignments)))
  }


  override def toString = {
     Iterator(ungrounded.name, argAssignments, completionTime).mkString("GroundedAction(",", ",")")
  }

  def applyStart(state: State) {
    ungrounded.effect.updateState(state, PDDL.Start, state.makeContext(argAssignments))
  }

  def applyEnd(state: State) {
    ungrounded.effect.updateState(state, PDDL.End, state.makeContext(argAssignments))
  }

}

object GroundedAction {
  implicit val ordering = new Ordering[GroundedAction] {
    def compare(x: GroundedAction, y: GroundedAction): Int = {
      -(x.completionTime - y.completionTime).toInt
    }
  }
}


