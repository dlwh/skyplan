package dlwh.skyplan

import collection.immutable.BitSet
import dlwh.skyplan.Expression.Resource
import dlwh.skyplan.PDDL.{AssignEffect, RefAssignEffect, Effect}
import breeze.util.Index


sealed trait IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier, context: EvalContext)
}

object NoEffect extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier, context: EvalContext) {}
}

object IndexedEffect {

  def fromEffect(effect: Effect,
                 locals: Index[String],
                 objs: GroundedObjects,
                 refFunctions: Grounding[String],
                 valFunctions: Grounding[String],
                 preds: Grounding[String]):IndexedEffect = {
    def rec(eff: Effect, varBindings: Index[String]):IndexedEffect = eff match {
      case PDDL.AndEffect(xs) =>
        val recs = xs.map(rec(_,varBindings))
        AndEffect(recs)
      case PDDL.DisablePred(pred) =>
        val predIndex = preds.index(pred.predicate)
        DisableDynamicPredicate(predIndex, pred.args.map(CellExpression.fromRefExp(_, refFunctions.index, varBindings, objs.index)))
      case PDDL.EnablePred(pred) =>
        val predIndex = preds.index(pred.predicate)
        EnableDynamicPredicate(predIndex, pred.args.map(CellExpression.fromRefExp(_, refFunctions.index, varBindings, objs.index)))
      case RefAssignEffect(lhs, rhs) =>
        val lcell = CellExpression.fromRefExp(lhs, refFunctions.index, varBindings, objs.index).asInstanceOf[Expression.Cell]
        val rcell = CellExpression.fromRefExp(rhs, refFunctions.index, varBindings, objs.index)
        AssignToCell(lcell,  rcell)
      case ae@AssignEffect(op, lhs, rhs) =>
        val ri = refFunctions.index(lhs.name)
        if(ri >= 0) {
          rec(ae.toRefAssignEffect, varBindings)
        } else {
          val lcell = Expression.fromValExp(lhs, refFunctions.index, valFunctions.index, varBindings, objs.index)
          val rcell = Expression.fromValExp(rhs, refFunctions.index, valFunctions.index, varBindings, objs.index)
          AssignToResource(op, lcell.asInstanceOf[Expression.Resource],  rcell)
        }
      case PDDL.UniversalEffect(args, eff) =>
        val extended = Index(varBindings ++ args.map(_.name))
        UniversalEffect(objs.allGroundingsOfArgumentTypes(args.map(_.tpe)).map(_.toArray), rec(eff, extended))
      case PDDL.TimedEffect(spec, arg) =>
        val a = rec(arg, varBindings)
        new TimedEffect(spec, a)
      case PDDL.CondEffect(guard, arg) =>
        val ig = IndexedCondition.fromCondition(guard, preds, refFunctions.index, valFunctions.index, varBindings, objs.index)
        val a = rec(arg, varBindings)
        new CondEffect(ig, a)
    }
    rec(effect, locals)
  }
}

case class EnableMultiple(preds: BitSet) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier,  context: EvalContext) {
    state.axioms |= preds
  }
}

case class DisableMultiple(preds: BitSet) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier, context: EvalContext) {
    state.axioms &~= preds
  }
}

case class AndEffect(conjuncts: IndexedSeq[IndexedEffect]) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier, context: EvalContext) {
    conjuncts foreach (_.updateState(state, time, context))
  }
}

case class DisableDynamicPredicate(predicateId: Int, args: IndexedSeq[CellExpression]) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier, context: EvalContext) {
    state.axioms -= state.problem.predicates.ground(predicateId, args.map(_.cell(context)))
  }
}

case class EnableDynamicPredicate(predicateId: Int, args: IndexedSeq[CellExpression]) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier,  context: EvalContext) {
    state.axioms += state.problem.predicates.ground(predicateId, args.map(_.cell(context)))
  }
}

case class AssignToResource(op: AssignOp, lhs: Resource, rhs: ValExpression) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier,  context: EvalContext) {
    val r = lhs.valueWith(context)
    val exp = rhs.valueWith(context)
    op match {
      case Assign =>
        lhs.update(context, exp)
      case Increase =>
        lhs.update(context, exp + r)
      case Decrease =>
        lhs.update(context, exp - r)
      case ScaleUp =>
        lhs.update(context, r * exp)
      case ScaleDown =>
        lhs.update(context, r / exp)
    }
  }
}

case class AssignToCell(lhs: Expression.Cell, rhs: CellExpression) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier,  context: EvalContext) {
    lhs.update(context, rhs.cell(context))
  }
}

case class TimedEffect(time: PDDL.TimeSpecifier, arg: IndexedEffect) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier,  context: EvalContext) {
    if(this.time == time)
      arg.updateState(state, time, context)
  }
}

case class CondEffect(guard: IndexedCondition, arg: IndexedEffect) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier,  context: EvalContext) {
    if(guard.holds(state, context)) {
      arg.updateState(state, time, context)
    }
  }
}

case class UniversalEffect(objs: IndexedSeq[Array[Int]], arg: IndexedEffect) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier,  context: EvalContext) {
    for(list <- objs) {
      arg.updateState(state, time, context.addLocals(list))
    }
  }
}

sealed trait AssignOp
case object Assign extends AssignOp
case object ScaleUp extends AssignOp
case object ScaleDown extends AssignOp
case object Increase extends AssignOp
case object Decrease extends AssignOp
