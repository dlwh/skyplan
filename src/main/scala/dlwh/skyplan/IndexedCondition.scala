package dlwh.skyplan

import dlwh.skyplan.PDDL.BinaryComp
import breeze.util.Index

sealed trait IndexedCondition {
  def holds(s: State, context: EvalContext):Boolean
}

object IndexedCondition {
  def fromCondition(cond: PDDL.Condition,
                    preds: Grounding[String],
                    refFunctions: Index[String],
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
              Expression.fromValExp(arg1, refFunctions, valFunctions, varBindings, globals),
              Expression.fromValExp(arg2, refFunctions, valFunctions, varBindings, globals))
          case PDDL.RComp(arg1, arg2) =>
            CellEqualCondition(
              CellExpression.fromRefExp(arg1, refFunctions, varBindings, globals),
              CellExpression.fromRefExp(arg2, refFunctions, varBindings, globals))
          case PDDL.Pred(name, args) =>
            val predIndex = preds.index(name)
            PredicateCondition(predIndex, args.map(CellExpression.fromRefExp(_, refFunctions, varBindings, globals)))

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
}

case class BinaryCompCondition(op: BinaryComp, lhs: ValExpression, rhs: ValExpression) extends IndexedCondition {
  def holds(s: State, context: EvalContext): Boolean = {
    op(lhs.valueWith(context), rhs.valueWith(context))
  }
}

case class CellEqualCondition(lhs: CellExpression, rhs: CellExpression) extends IndexedCondition {
  def holds(s: State, context: EvalContext): Boolean = {
    lhs.cell(context) == rhs.cell(context)
  }
}

case class PredicateCondition(predicateId: Int, args: IndexedSeq[CellExpression]) extends IndexedCondition {
  def holds(s: State, context: EvalContext): Boolean = {
    s.axioms(s.problem.predicates.ground(predicateId, args.map(_.cell(context))))
  }
}