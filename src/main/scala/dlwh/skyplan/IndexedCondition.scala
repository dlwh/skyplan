package dlwh.skyplan

import dlwh.skyplan.PDDL.BinaryComp
import breeze.util.Index
import breeze.linalg.HashVector
import collection.immutable.BitSet

sealed trait IndexedCondition {

  def holds(s: State, context: EvalContext):Boolean
  def resourceSummary(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary
  def computeMissingSummary(state: State): ResourceSummary
}

object IndexedCondition {
  def fromCondition(cond: PDDL.Condition,
                    preds: Grounding[String],
                 valFunctions: Index[String],
                 locals: Index[String],
                 globals: Index[String]):IndexedCondition = {
    def rec(cond: PDDL.Condition, varBindings: Index[String]):IndexedCondition =  {
      try {
        cond match {
          case PDDL.AndCondition(conjuncts) =>
            AndCondition(conjuncts.map(rec(_, varBindings)))
          case PDDL.FComp(comp, arg1, arg2) =>
            BinaryCompCondition(comp,
              Expression.fromValExp(arg1, valFunctions, varBindings, globals),
              Expression.fromValExp(arg2, valFunctions, varBindings, globals))
          case PDDL.RComp(arg1, arg2) =>
            CellEqualCondition(
              CellExpression.fromRefExp(arg1, varBindings, globals),
              CellExpression.fromRefExp(arg2, varBindings, globals))
          case PDDL.Pred(name, args) =>
            val predIndex = preds.index(name)
            PredicateCondition(predIndex, args.map(CellExpression.fromRefExp(_, varBindings, globals)))

        }
      } catch {
        case e: Exception =>
          throw new RuntimeException("While handling " + cond, e)
      }
    }

    rec(cond, locals)
  }
}

case class AndCondition(conjuncts: IndexedSeq[IndexedCondition]) extends IndexedCondition {
  def holds(s: State, context: EvalContext):Boolean = {
    conjuncts.forall(_.holds(s,context))
  }


  def resourceSummary(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = conjuncts.map(_.resourceSummary(inst, args)).reduce( _ ++ _ )

  def computeMissingSummary(state: State): ResourceSummary = conjuncts.map(_.computeMissingSummary(state)).reduce(_ ++ _)
}

case class OrCondition(disjuncts: IndexedSeq[IndexedCondition]) extends IndexedCondition {
  def holds(s: State, context: EvalContext):Boolean = {
    disjuncts.exists(_.holds(s,context))
  }

  def resourceSummary(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = {
    disjuncts.map(_.resourceSummary(inst, args)).reduce(_ ++ _)
  }

  def computeMissingSummary(state: State): ResourceSummary = {
    if(!holds(state, state.makeContext())) ResourceSummary.empty
    else resourceSummary(state.problem, IndexedSeq.empty)
  }
}

case class NotCondition(base: IndexedCondition) extends IndexedCondition {
  def holds(s: State, context: EvalContext):Boolean = {
    !base.holds(s, context)
  }


  def resourceSummary(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = {
    base.resourceSummary(inst, args).flip
  }

  def computeMissingSummary(state: State): ResourceSummary = {
    if(!holds(state, state.makeContext())) ResourceSummary.empty
    else resourceSummary(state.problem, IndexedSeq.empty)
  }
}

case class BinaryCompCondition(op: BinaryComp, lhs: ValExpression, rhs: ValExpression) extends IndexedCondition {
  def holds(s: State, context: EvalContext): Boolean = {
    op(lhs.valueWith(context), rhs.valueWith(context))
  }

  def resourceSummary(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = {
    val (lhsPos, lhsNeg) = lhs.getResourceSigns(inst, args)
    val (rhsPos, rhsNeg) = rhs.getResourceSigns(inst, args)
    op match {
      case PDDL.<= | PDDL.< =>
        ResourceSummary(lhsNeg ++ rhsPos, lhsPos ++ rhsNeg)
      case PDDL.Equals =>
        val all = lhsPos ++ lhsNeg ++ rhsPos ++ rhsNeg
        ResourceSummary(all, all)
      case PDDL.>= | PDDL.> =>
        ResourceSummary(rhsNeg ++ lhsPos, rhsPos ++ lhsNeg)
    }
  }

  def computeMissingSummary(state: State): ResourceSummary = {
    if(holds(state, state.makeContext())) ResourceSummary.empty
    else resourceSummary(state.problem, IndexedSeq.empty)
  }
}

case class CellEqualCondition(lhs: CellExpression, rhs: CellExpression) extends IndexedCondition {
  def holds(s: State, context: EvalContext): Boolean = {
    lhs.cell(context) == rhs.cell(context)
  }
  def resourceSummary(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = ResourceSummary.empty

  def computeMissingSummary(state: State): ResourceSummary = ResourceSummary.empty
}

case class PredicateCondition(predicateId: Int, args: IndexedSeq[CellExpression]) extends IndexedCondition {
  def holds(s: State, context: EvalContext): Boolean = {
    s.axioms(s.problem.predicates.ground(predicateId, args.map(_.cell(context))))
  }

  def resourceSummary(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = {
    val context = EvalContext.onlyLocals(args)
    ResourceSummary(addedAxioms=BitSet(inst.predicates.ground(predicateId, this.args.map(_.cell(context)))))
  }

  def computeMissingSummary(state: State): ResourceSummary = {
    if(holds(state, state.makeContext())) ResourceSummary.empty
    else resourceSummary(state.problem, IndexedSeq.empty)
  }
}