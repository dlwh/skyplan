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
  val timeOrder = resourceOrders(problem.valFuns.ground(problem.totalTimeIndex, IndexedSeq()))
  val (goodAxioms, badAxioms, crazyAxioms) = {
    val byType = (0 until axiomOrders.length).groupBy(axiomOrders)
    val good = byType.get(GoodAxiom).map(mutable.BitSet.empty ++ _).getOrElse(mutable.BitSet.empty).toJavaBitSet
    val bad = byType.get(BadAxiom).map(mutable.BitSet.empty ++ _).getOrElse(mutable.BitSet.empty).toJavaBitSet
    val crazy = byType.get(CrazyAxiom).map(mutable.BitSet.empty ++ _).getOrElse(mutable.BitSet.empty).toJavaBitSet

    (good, bad, crazy)
  }
//  for (i <- 0 until problem.valFuns.size) {
//    println(problem.valFuns.groundedIndex.get(i) + "(" + i + "): " + resourceOrders(i))
//  }
//  for (i <- 0 until problem.predicates.size) {
//    println(problem.predicates.groundedIndex.get(i) + "(" + i + "): " + axiomOrders(i))
//  }

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
      case Multi(Plus, args) => args.map(allVars(_, flip, c)).foldLeft(IndexedSeq.empty[(Int, Boolean)])(_ ++ _)
      case Multi(Times, args) => {
        if (args.contains(Number(0))) mutable.IndexedSeq.empty[(Int, Boolean)]
        else args.map(allVars(_, flip, c)).foldLeft(IndexedSeq.empty[(Int, Boolean)])(_ ++ _)
      }
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
    applyOrderingsForCondition(problem.goal, EvalContext.emptyContext, ro, ao)
    applyToAll(ro, LessIsBetter, allVars(problem.metricExp, false, EvalContext.emptyContext))
    for ((action, actionArgs) <- problem.allGroundedActions) {
      for (pc <- action.precondition) {
        val context = new EvalContext {
          def local(i: Int) = actionArgs(i)
          def numLocals = actionArgs.size

          def resource(fn: Int, args: IndexedSeq[Int]) = 0.0

          def constResource(fn: Int, args: IndexedSeq[Int]): Double = 0.0

          def updateResource(fn: Int, args: IndexedSeq[Int], v: Double) {}
          def cell(fn: Int, args: IndexedSeq[Int]) = -1
          def updateCell(fn: Int, args: IndexedSeq[Int], v: Int) {}
        }
        applyOrderingsForCondition(pc, context, ro, ao)
      }
    }

    (ro, ao)
  }


  def applyOrderingsForCondition(c: IndexedCondition, context: EvalContext,
                                 ro: Array[ResourceOrdering], ao: Array[AxiomOrdering]) {
    for ((baseCondition, flip) <- baseConditions(c, false)) {
      baseCondition match {
        case BinaryCompCondition(op, lhs, rhs) => {
          val varsOnLHS = allVars(lhs, flip, context)
          val varsOnRHS = allVars(rhs, flip, context)
          op match {
            case PDDL.Equals => applyToAll(ro, NoOrder, varsOnLHS ++ varsOnRHS)
            case PDDL.> => {
              applyToAll(ro, MoreIsBetter, varsOnLHS);
              applyToAll(ro, LessIsBetter, varsOnRHS)
            }
            case PDDL.>= => {
              applyToAll(ro, MoreIsBetter, varsOnLHS);
              applyToAll(ro, LessIsBetter, varsOnRHS)
            }
            case PDDL.< => {
              applyToAll(ro, MoreIsBetter, varsOnRHS);
              applyToAll(ro, LessIsBetter, varsOnLHS)
            }
            case PDDL.<= => {
              applyToAll(ro, MoreIsBetter, varsOnRHS);
              applyToAll(ro, LessIsBetter, varsOnLHS)
            }
          }
        }
        case PredicateCondition(predicateId, args) => {
          val axiom = problem.predicates.ground(predicateId, args.map(_.cell(context)))
          var currentOrder: AxiomOrdering = GoodAxiom
          if (flip) currentOrder = currentOrder.flip
          ao(axiom) = currentOrder combine ao(axiom)
        }
        case _ => {} // this shouldn't actually happen
      }
    }
  }

  def adjustForActionEffects(action: Int, order: PartialOrder): PartialOrder = {
    if (assumePositiveActionEffects) order
    else breeze.util.TODO
  }

  def isDominatedBy(first: State, second: State): Boolean = {
    val cmp = compareStates(first, second, shortCircuitOnDominates = true)
    cmp == IsDominated
  }

  def isEqualToOrDominatedBy(first: State, second: State): Boolean = {
    val cmp = compareStates(first, second, shortCircuitOnDominates = true)
    cmp == IsDominated || cmp == Equals
  }

  def compareStates(first: State, second: State, shortCircuitOnDominates: Boolean = false) : PartialOrder = {
    var cmp : PartialOrder = Equals
    if(timeOrder != null) {
      cmp = cmp combine timeOrder.orderResourceQuantities(first.time, second.time)
      if (cmp == NonComparable) return cmp
      if (shortCircuitOnDominates && cmp == Dominates) return cmp
    }

    // if they differ at all.
    if(first.axioms != second.axioms) {
      val first_xor_second = (first.axioms ^ second.axioms)
      if((first_xor_second & crazyAxioms).nonEmpty) return NonComparable

      val first_minus_second = (first.axioms &~ second.axioms)
      val second_minus_first = (second.axioms &~ first.axioms)

      val extraGood = (first_minus_second & goodAxioms).nonEmpty
      val fewerGood = (second_minus_first & goodAxioms).nonEmpty
      val extraBad = (first_minus_second & badAxioms).nonEmpty
      val fewerBad = (second_minus_first & badAxioms).nonEmpty
      if( (extraGood && fewerGood) || (extraBad && fewerBad) ) {
        cmp = NonComparable
      } else if(extraGood && !extraBad) {
        cmp = cmp combine Dominates
      } else if(fewerGood && !fewerBad) {
        cmp = cmp combine IsDominated
      } else {
        cmp = NonComparable
      }
    }

    if (cmp == NonComparable) return cmp
    if (shortCircuitOnDominates && cmp == Dominates) return cmp

    val visited = collection.mutable.BitSet.empty
    var i = 0
    while(i < first.resources.iterableSize) {
      if(first.resources.isActive(i)) {
        val r = first.resources.index(i)
        val v1 = first.resources.data(i)
        visited += r
        val o = resourceOrders(r)
        if (o != null) {
          cmp = cmp combine o.orderResourceQuantities(v1, second.resources(r))
          if (cmp == NonComparable) return cmp
          if (shortCircuitOnDominates && cmp == Dominates) return cmp
        }
      }
      i += 1
    }

    i = 0
    while(i < second.resources.iterableSize) {
      if(second.resources.isActive(i)) {
        val r = second.resources.index(i)
        if(!visited(r)) {
          val v2 = second.resources.data(i)
          val o = resourceOrders(r)
          if (o != null) {
            cmp = cmp combine o.orderResourceQuantities(0, v2)
            if (cmp == NonComparable) return cmp
            if (shortCircuitOnDominates && cmp == Dominates) return cmp
          }
        }
      }
      i += 1
    }

    i = 0
    while(i < first.pendingActions.data.iterableSize) {
      if(first.pendingActions.data.isActive(i)) {
        val action = first.pendingActions.data.indexAt(i)
          visited += action
          cmp = checkActionForDominance(first, action, second, cmp)
          if (cmp == NonComparable) return cmp
          if (shortCircuitOnDominates && cmp == Dominates) return cmp

      }
      i += 1
    }
    i = 0
    while(i < second.pendingActions.data.iterableSize) {
      if(second.pendingActions.data.isActive(i)) {
        val action = second.pendingActions.data.indexAt(i)
        if(!visited(action)) {
          cmp = checkActionForDominance(first, action, second, cmp)
          if (cmp == NonComparable) return cmp
          if (shortCircuitOnDominates && cmp == Dominates) return cmp
        }
      }
      i += 1
    }


    cmp
  }

  private def checkActionForDominance(first: State, action: Int, second: State, _cmp: PartialOrder): PartialOrder = {
    var cmp: PartialOrder = _cmp
    val q1 = if (first.pendingActions.data.contains(action)) first.pendingActions.data(action).clone()
    else mutable.Queue.empty[Double]
    val q2 = if (second.pendingActions.data.contains(action)) second.pendingActions.data(action).clone()
    else mutable.Queue.empty[Double]
    var actionCmp: PartialOrder = Equals
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
      } else {
        // next1 > next2
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
    cmp
  }
}