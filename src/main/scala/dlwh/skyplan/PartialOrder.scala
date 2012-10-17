package dlwh.skyplan

import dlwh.skyplan.Expression._
import collection.mutable
import dlwh.skyplan.Expression.Negation
import dlwh.skyplan.Expression.Binary
import dlwh.skyplan.Expression.Resource
import dlwh.skyplan.Expression.Multi

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
  def combine(other: ResourceOrdering) = {
    if (other == null) this
    else if (this != other) NoOrder
    else this
  }
  def flip : ResourceOrdering
  def orderResourceQuantities(first: Double, second: Double) : PartialOrder
}
case object MoreIsBetter extends ResourceOrdering {
  def flip = LessIsBetter
  def orderResourceQuantities(first: Double, second: Double) = {
    if (first == second) Equals
    else if (first > second) Dominates
    else IsDominated // first < second
  }
}
case object LessIsBetter extends ResourceOrdering {
  def flip = MoreIsBetter
  def orderResourceQuantities(first: Double, second: Double) = {
    if (first == second) Equals
    else if (first < second) Dominates
    else IsDominated // first > second
  }
}
case object NoOrder extends ResourceOrdering {
  def flip = NoOrder
  def orderResourceQuantities(first: Double, second: Double) = {
    if (first == second) Equals
    else NonComparable
  }
}

sealed trait AxiomOrdering {
  def combine(other: AxiomOrdering) = {
    if (other == null) this
    else if (this != other) CrazyAxiom
    else this
  }
  def flip : AxiomOrdering
  def orderAxiomTruth(first: Boolean, second: Boolean) : PartialOrder
}
case object GoodAxiom extends AxiomOrdering {
  def flip = BadAxiom
  def orderAxiomTruth(first: Boolean, second: Boolean) = {
    if (first == second) Equals
    else if (first) Dominates
    else IsDominated
  }
}
case object BadAxiom extends AxiomOrdering {
  def flip = GoodAxiom
  def orderAxiomTruth(first: Boolean, second: Boolean) = {
    if (first == second) Equals
    else if (second) Dominates
    else IsDominated
  }
}
case object CrazyAxiom extends AxiomOrdering {
  def flip = CrazyAxiom
  def orderAxiomTruth(first: Boolean, second: Boolean) = {
    if (first == second) Equals
    else NonComparable
  }
}

case class DominanceChecker(problem: ProblemInstance, assumePositiveActionEffects: Boolean = true) {
  val (resourceOrders, axiomOrders) = inferOrderings

  def baseConditions(condition: IndexedCondition, flip: Boolean): Seq[(IndexedCondition, Boolean)] = {
    condition match {
      case AndCondition(conjuncts) =>
        conjuncts.map(baseConditions(_, flip)).foldLeft(IndexedSeq.empty[(IndexedCondition, Boolean)])(_ ++ _)
      case OrCondition(disjuncts) =>
        disjuncts.map(baseConditions(_, flip)).foldLeft(IndexedSeq.empty[(IndexedCondition, Boolean)])(_ ++ _)
      case NotCondition(base) => baseConditions(base, !flip)
      case BinaryCompCondition(op, lhs, rhs) => IndexedSeq((condition, flip))
      case PredicateCondition(id, args) => IndexedSeq((condition, flip))
      case _ => IndexedSeq.empty[(IndexedCondition, Boolean)]
    }
  }

  def allVars(expression: ValExpression, flip: Boolean, c: EvalContext) : IndexedSeq[(Int, Boolean)] = {
    expression match {
      case Multi(op, args) => args.map(allVars(_, flip, c)).foldLeft(IndexedSeq.empty[(Int, Boolean)])(_ ++ _)
      case Binary(Plus, lhs, rhs) => allVars(lhs, flip, c) ++ allVars(rhs, flip, c)
      case Binary(Times, Number(0), rhs) => mutable.IndexedSeq.empty[(Int, Boolean)]
      case Binary(Times, lhs, Number(0)) => mutable.IndexedSeq.empty[(Int, Boolean)]
      case Binary(Times, lhs, rhs) => allVars(lhs, flip, c) ++ allVars(rhs, flip, c)
      case Binary(Minus, lhs, rhs) => allVars(lhs, flip, c) ++ allVars(rhs, !flip, c)
      case Binary(Div, lhs, rhs) => allVars(lhs, flip, c) ++ allVars(rhs, !flip, c)
      case Negation(arg) => allVars(arg, !flip, c)
      case Resource(fn, args) => IndexedSeq((problem.valFuns.ground(fn, args.map(_.cell(c))), flip))
      case _ => mutable.IndexedSeq.empty[(Int, Boolean)]
    }
  }

  def applyToAll(ro: Array[ResourceOrdering], order: ResourceOrdering, vars: IndexedSeq[(Int, Boolean)]) {
    for ((r, flip) <- vars) {
      val orderToApply = if (flip) order.flip else order
      ro(r) = orderToApply combine ro(r)
    }
  }

  def inferOrderings : (Array[ResourceOrdering], Array[AxiomOrdering]) = {
    val ro = new Array[ResourceOrdering](problem.valFuns.size)
    val ao = new Array[AxiomOrdering](problem.predicates.size)
    applyToAll(ro, LessIsBetter, allVars(problem.metricExp, false, EvalContext.emptyContext))
    for ((action, actionArgs) <- problem.allGroundedActions) {
      for (pc <- action.precondition) {
        val context = new EvalContext {
          def local(i: Int) = actionArgs(i)
          def numLocals = actionArgs.size

          def resource(fn: Int, args: IndexedSeq[Int]) = 0.0
          def updateResource(fn: Int, args: IndexedSeq[Int], v: Double) {}
          def cell(fn: Int, args: IndexedSeq[Int]) = -1
          def updateCell(fn: Int, args: IndexedSeq[Int], v: Int) {}
        }
        for ((baseCondition, flip) <- baseConditions(pc, false)) {
          baseCondition match {
            case BinaryCompCondition(op, lhs, rhs) => {
              val varsOnLHS = allVars(lhs, flip, context)
              val varsOnRHS = allVars(rhs, flip, context)
              op match {
                case PDDL.Equals => applyToAll(ro, NoOrder, varsOnLHS ++ varsOnRHS)
                case PDDL.> => { applyToAll(ro, MoreIsBetter, varsOnLHS); applyToAll(ro, LessIsBetter, varsOnRHS) }
                case PDDL.>= => { applyToAll(ro, MoreIsBetter, varsOnLHS); applyToAll(ro, LessIsBetter, varsOnRHS) }
                case PDDL.< => { applyToAll(ro, MoreIsBetter, varsOnRHS); applyToAll(ro, LessIsBetter, varsOnLHS) }
                case PDDL.<= => { applyToAll(ro, MoreIsBetter, varsOnRHS); applyToAll(ro, LessIsBetter, varsOnLHS) }
              }
            }
            case PredicateCondition(predicateId, args) => {
              val axiom = problem.predicates.ground(predicateId, args.map(_.cell(context)))
              var currentOrder : AxiomOrdering = GoodAxiom
              if (flip) currentOrder = currentOrder.flip
              ao(axiom) = currentOrder combine ao(axiom)
            }
            case _ => {} // this shouldn't actually happen
          }
        }
      }
    }

    (ro, ao)
  }


  def adjustForActionEffects(action: Int, order: PartialOrder): PartialOrder = {
    if (assumePositiveActionEffects) order
    else breeze.util.TODO
  }

  def compareStates(first: State, second: State) : PartialOrder = {
    var cmp : PartialOrder = LessIsBetter.orderResourceQuantities(first.time, second.time)
    for (r : Int <- first.resources.keySet union second.resources.keySet) {
      val o = resourceOrders(r)
      if (o != null) {
        cmp = cmp combine o.orderResourceQuantities(first.resources(r), second.resources(r))
        if (cmp == NonComparable) return cmp
      }
    }
    for (a : Int <- first.axioms | second.axioms) {
      val o = axiomOrders(a)
      if (o != null) {
        cmp = cmp combine o.orderAxiomTruth(first.axioms.contains(a), second.axioms.contains(a))
        if (cmp == NonComparable) return cmp
      }
    }

    for (action : Int <- mutable.BitSet.empty ++ first.pendingActions.data.activeKeysIterator ++
      second.pendingActions.data.activeKeysIterator) {
      val q1 = first.pendingActions.data(action).clone()
      val q2 = second.pendingActions.data(action).clone()
      var actionCmp : PartialOrder = Equals
      var counter = 0
      while (!(q1.isEmpty || q2.isEmpty)) {
        val next1 = q1.head
        val next2 = q2.head
        if (next1 == next2) {
          q1.dequeue()
          q2.dequeue()
        } else if (next1 < next2) {
          counter -= 1
          if (counter < 0) actionCmp = actionCmp combine Dominates
          q1.dequeue()
        } else { // next1 > next2
          counter += 1
          if (counter > 0) actionCmp = actionCmp combine IsDominated
          q2.dequeue()
        }
      }
      if (!q1.isEmpty) {
        counter -= 1
        if (counter < 0) actionCmp = actionCmp combine Dominates
      }
      if (!q2.isEmpty) {
        counter += 1
        if (counter > 0) actionCmp = actionCmp combine IsDominated
      }
      actionCmp = adjustForActionEffects(action, actionCmp)
      cmp = cmp combine actionCmp
      if (cmp == NonComparable) return cmp
    }

    cmp
  }
}