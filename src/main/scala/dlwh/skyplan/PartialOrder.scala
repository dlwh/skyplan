package dlwh.skyplan

/**
 * Created with IntelliJ IDEA.
 * User: dburkett
 * Date: 10/12/12
 * Time: 1:33 PM
 * To change this template use File | Settings | File Templates.
 */
sealed trait PartialOrder {
  def combine(other: PartialOrder) = {
    if(this == Equals) other
    else if (other == Equals) this
    else if (this == NonComparable || other == NonComparable) NonComparable
    else if (this != other) NonComparable
    else this
  }
}
case object Equals extends PartialOrder
case object Dominates extends PartialOrder
case object IsDominated extends PartialOrder
case object NonComparable extends PartialOrder

sealed trait ResourceOrdering {
  def orderResourceQuantities(first: Double, second: Double) : PartialOrder
}
case object MoreIsBetter extends ResourceOrdering {
  def orderResourceQuantities(first: Double, second: Double) = {
    if (first == second) Equals
    else if (first > second) Dominates
    else IsDominated // first < second
  }
}
case object LessIsBetter extends ResourceOrdering {
  def orderResourceQuantities(first: Double, second: Double) = {
    if (first == second) Equals
    else if (first < second) Dominates
    else IsDominated // first > second
  }
}
case object NoOrder extends ResourceOrdering {
  def orderResourceQuantities(first: Double, second: Double) = {
    if (first == second) Equals
    else NonComparable
  }
}

sealed trait AxiomOrdering {
  def orderAxiomTruth(first: Boolean, second: Boolean) : PartialOrder
}
case object GoodAxiom extends AxiomOrdering {
  def orderAxiomTruth(first: Boolean, second: Boolean) = {
    if (first == second) Equals
    else if (first) Dominates
    else IsDominated
  }
}
case object BadAxiom extends AxiomOrdering {
  def orderAxiomTruth(first: Boolean, second: Boolean) = {
    if (first == second) Equals
    else if (second) Dominates
    else IsDominated
  }
}
case object CrazyAxiom extends AxiomOrdering {
  def orderAxiomTruth(first: Boolean, second: Boolean) = {
    if (first == second) Equals
    else NonComparable
  }
}

case class DominanceChecker(problem: ProblemInstance) {
  val (resourceOrders, axiomOrders) = inferOrderings

  def inferOrderings : (Array[ResourceOrdering], Array[AxiomOrdering]) = {
    val ro = new Array[ResourceOrdering](problem.valFuns.size)
    val ao = new Array[AxiomOrdering](problem.predicates.size)

    (ro, ao)
  }


  def compareStates(first: State, second: State) : PartialOrder = {
    var cmp : PartialOrder = LessIsBetter.orderResourceQuantities(first.time, second.time)
    for (r : Int <- first.resources.keySet union second.resources.keySet) {
      val o = resourceOrders(r)
      cmp = cmp combine o.orderResourceQuantities(first.resources(r), second.resources(r))
      if (cmp == NonComparable) return cmp
    }
    for (a : Int <- first.axioms | second.axioms) {
      val o = axiomOrders(a)
      cmp = cmp combine o.orderAxiomTruth(first.axioms.contains(a), second.axioms.contains(a))
      if (cmp == NonComparable) return cmp
    }

    cmp
  }
}