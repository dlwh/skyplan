package dlwh.skyplan

import dlwh.skyplan.PDDL.{Start, BinaryComp}
import breeze.util.Index
import breeze.linalg.HashVector
import collection.immutable.BitSet

sealed trait IndexedCondition {

  def holds(s: State, context: EvalContext):Boolean
  def resourceSummary(inst: ProblemInstance, args: Array[Int]): ResourceSummary
  def computeMissingSummary(state: State): ResourceSummary
}

object IndexedCondition {
  def fromCondition(cond: PDDL.Condition,
                    preds: Grounding[String],
                    constPreds: Grounding[String],
                    resources: Index[String],
                    constResources: Index[String],
                    locals: Index[String],
                    globals: Index[String]):(Option[IndexedCondition], Option[IndexedCondition]) = {
    def rec(cond: PDDL.Condition, varBindings: Index[String]):(Option[IndexedCondition], Option[IndexedCondition]) =  {
      try {
        cond match {
          case PDDL.TimedCondition(Start, base) =>
            (rec(base, varBindings)._1, None)
          case PDDL.ContinuousCondition(base) =>
            (None, rec(base, varBindings)._1)
          case PDDL.AndCondition(conjuncts) => {
            val (pre, cont) = conjuncts.map(rec(_, varBindings)).foldLeft((IndexedSeq.empty[IndexedCondition], IndexedSeq.empty[IndexedCondition]))((s, p) => (s._1 ++ p._1, s._2 ++ p._2))
            (conjoin(pre), conjoin(cont))
          }
          case PDDL.FComp(comp, arg1, arg2) =>
            (Some(BinaryCompCondition(comp,
              Expression.fromValExp(arg1, resources, constResources, varBindings, globals),
              Expression.fromValExp(arg2, resources, constResources, varBindings, globals))), None)
          case PDDL.RComp(arg1, arg2) =>
            (Some(CellEqualCondition(
              CellExpression.fromRefExp(arg1, varBindings, globals),
              CellExpression.fromRefExp(arg2, varBindings, globals))), None)
          case PDDL.Pred(name, args) =>
            val predIndex = preds.index(name)
            if(predIndex >= 0)
              (Some(PredicateCondition(predIndex, args.toArray.map(CellExpression.fromRefExp(_, varBindings, globals)))), None)
            else
              (Some(ConstPredicateCondition(constPreds.index(name), args.toArray.map(CellExpression.fromRefExp(_, varBindings, globals)))), None)
        }
      } catch {
        case e: Exception =>
          throw new RuntimeException("While handling " + cond, e)
      }
    }

    def conjoin(seq: IndexedSeq[IndexedCondition]) = {
      if (seq.isEmpty) None else Some(AndCondition(seq.toArray))
    }

    rec(cond, locals)
  }
}

case class AndCondition(conjuncts: Array[IndexedCondition]) extends IndexedCondition {
  def holds(s: State, context: EvalContext):Boolean = {
    conjuncts.forall(_.holds(s,context))
  }


  def resourceSummary(inst: ProblemInstance, args: Array[Int]): ResourceSummary = conjuncts.map(_.resourceSummary(inst, args)).reduce( _ ++ _ )

  def computeMissingSummary(state: State): ResourceSummary = conjuncts.map(_.computeMissingSummary(state)).reduce(_ ++ _)
}

case class OrCondition(disjuncts: Array[IndexedCondition]) extends IndexedCondition {
  def holds(s: State, context: EvalContext):Boolean = {
    disjuncts.exists(_.holds(s,context))
  }

  def resourceSummary(inst: ProblemInstance, args: Array[Int]): ResourceSummary = {
    disjuncts.map(_.resourceSummary(inst, args)).reduce(_ ++ _)
  }

  def computeMissingSummary(state: State): ResourceSummary = {
    if(!holds(state, state.makeContext())) ResourceSummary.empty
    else resourceSummary(state.problem, Array.empty)
  }
}

case class NotCondition(base: IndexedCondition) extends IndexedCondition {
  def holds(s: State, context: EvalContext):Boolean = {
    !base.holds(s, context)
  }


  def resourceSummary(inst: ProblemInstance, args: Array[Int]): ResourceSummary = {
    base.resourceSummary(inst, args).flip
  }

  def computeMissingSummary(state: State): ResourceSummary = {
    if(!holds(state, state.makeContext())) ResourceSummary.empty
    else resourceSummary(state.problem, Array.empty)
  }
}

case class BinaryCompCondition(op: BinaryComp, lhs: ValExpression, rhs: ValExpression) extends IndexedCondition {
  def holds(s: State, context: EvalContext): Boolean = {
    op(lhs.valueWith(context), rhs.valueWith(context))
  }

  def resourceSummary(inst: ProblemInstance, args: Array[Int]): ResourceSummary = {
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
    else resourceSummary(state.problem, Array.empty)
  }
}

case class CellEqualCondition(lhs: CellExpression, rhs: CellExpression) extends IndexedCondition {
  def holds(s: State, context: EvalContext): Boolean = {
    lhs.cell(context) == rhs.cell(context)
  }
  def resourceSummary(inst: ProblemInstance, args: Array[Int]): ResourceSummary = ResourceSummary.empty

  def computeMissingSummary(state: State): ResourceSummary = ResourceSummary.empty
}

case class PredicateCondition(predicateId: Int, args: Array[CellExpression]) extends IndexedCondition {
  def holds(s: State, context: EvalContext): Boolean = {
    s.axioms(s.problem.predicates.ground(predicateId, args.map(_.cell(context))))
  }

  def resourceSummary(inst: ProblemInstance, args: Array[Int]): ResourceSummary = {
    val context = EvalContext.onlyLocals(args)
    ResourceSummary(addedAxioms=BitSet(inst.predicates.ground(predicateId, this.args.map(_.cell(context)))))
  }

  def computeMissingSummary(state: State): ResourceSummary = {
    if(holds(state, state.makeContext())) ResourceSummary.empty
    else resourceSummary(state.problem, Array.empty)
  }
}


case class ConstPredicateCondition(predicateId: Int, args: Array[CellExpression]) extends IndexedCondition {
  def holds(s: State, context: EvalContext): Boolean = {
    s.problem.constAxioms(s.problem.constantPredicates.ground(predicateId, args.map(_.cell(context))))
  }

  def resourceSummary(inst: ProblemInstance, args: Array[Int]): ResourceSummary = {
    ResourceSummary.empty
  }

  def computeMissingSummary(state: State): ResourceSummary = {
    ResourceSummary.empty
  }
}


object TrueCondition extends IndexedCondition {
  def holds(s: State, context: EvalContext): Boolean = true

  def resourceSummary(inst: ProblemInstance, args: Array[Int]): ResourceSummary = ResourceSummary.empty

  def computeMissingSummary(state: State): ResourceSummary = ResourceSummary.empty
}