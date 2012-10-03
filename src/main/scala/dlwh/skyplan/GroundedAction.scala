package dlwh.skyplan


case class GroundedAction(ungrounded: IndexedAction,
                          argAssignments: IndexedSeq[Int],
                          completionTime: Double) {

  override def toString = {
     Iterator(ungrounded.name, argAssignments, completionTime).mkString("GroundedAction(",", ",")")
  }

}

object GroundedAction {
  implicit val ordering = new Ordering[GroundedAction] {
    def compare(x: GroundedAction, y: GroundedAction): Int = {
      (x.completionTime - y.completionTime).toInt
    }
  }
}


