package dlwh.skyplan

import collection.immutable.BitSet
import dlwh.skyplan.Expression.Resource
import dlwh.skyplan.PDDL.{AssignEffect, Effect}
import breeze.util.Index
import breeze.linalg.Axis._0


sealed trait IndexedEffect {

  def updateState(state: State, time: PDDL.TimeSpecifier, context: EvalContext)

  /**
   * Returns
   * @param args
   */
  def possibleDelta(inst: ProblemInstance, args: IndexedSeq[Int]):ResourceSummary
}



object IndexedEffect {

  def fromEffect(effect: Effect,
                 locals: Index[String],
                 objs: GroundedObjects,
                 valFunctions: Grounding[String],
                 preds: Grounding[String]):IndexedEffect = {
    def rec(eff: Effect, varBindings: Index[String]):IndexedEffect = eff match {
      case PDDL.AndEffect(xs) =>
        val recs = xs.map(rec(_,varBindings))
        AndEffect(recs)
      case PDDL.DisablePred(pred) =>
        val predIndex = preds.index(pred.predicate)
        DisableDynamicPredicate(predIndex, pred.args.map(CellExpression.fromRefExp(_, varBindings, objs.index)))
      case PDDL.EnablePred(pred) =>
        val predIndex = preds.index(pred.predicate)
        EnableDynamicPredicate(predIndex, pred.args.map(CellExpression.fromRefExp(_, varBindings, objs.index)))
      case ae@AssignEffect(op, lhs, rhs) =>
        val lcell = Expression.fromValExp(lhs, valFunctions.index, varBindings, objs.index)
        val rcell = Expression.fromValExp(rhs, valFunctions.index, varBindings, objs.index)
        AssignToResource(op, lcell.asInstanceOf[Expression.Resource],  rcell)
      case PDDL.UniversalEffect(args, eff) =>
        val extended = Index(varBindings ++ args.map(_.name))
        UniversalEffect(objs.allGroundingsOfArgumentTypes(args.map(_.tpe)).map(_.toArray), rec(eff, extended))
      case PDDL.TimedEffect(spec, arg) =>
        val a = rec(arg, varBindings)
        new TimedEffect(spec, a)
      case PDDL.CondEffect(guard, arg) =>
        val ig = IndexedCondition.fromCondition(guard, preds, valFunctions.index, varBindings, objs.index)
        val a = rec(arg, varBindings)
        new CondEffect(ig, a)
    }
    rec(effect, locals)
  }
}

object NoEffect extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier, context: EvalContext) {}

  def possibleDelta(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = ResourceSummary.empty
}

case class EnableMultiple(preds: BitSet) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier,  context: EvalContext) {
    state.axioms |= preds
  }

  /**
   * Returns
   * @param args
   */
  def possibleDelta(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = ResourceSummary(addedAxioms = preds)
}

case class DisableMultiple(preds: BitSet) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier, context: EvalContext) {
    state.axioms &~= preds
  }

  def possibleDelta(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = {
    ResourceSummary(consumedAxioms = preds)
  }
}

case class AndEffect(conjuncts: IndexedSeq[IndexedEffect]) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier, context: EvalContext) {
    conjuncts foreach (_.updateState(state, time, context))
  }
  def possibleDelta(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = {
    conjuncts.map(_.possibleDelta(inst, args)).reduceLeft(_ ++ _)
  }
}

case class DisableDynamicPredicate(predicateId: Int, args: IndexedSeq[CellExpression]) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier, context: EvalContext) {
    state.axioms -= state.problem.predicates.ground(predicateId, args.map(_.cell(context)))
  }

  def possibleDelta(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = {
    val context = EvalContext.onlyLocals(args)
    ResourceSummary(consumedAxioms=BitSet(inst.predicates.ground(predicateId, this.args.map(_.cell(context)))))

  }
}

case class EnableDynamicPredicate(predicateId: Int, args: IndexedSeq[CellExpression]) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier,  context: EvalContext) {
    state.axioms += state.problem.predicates.ground(predicateId, args.map(_.cell(context)))
  }

  def possibleDelta(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = {
    val context = EvalContext.onlyLocals(args)
    ResourceSummary(addedAxioms=BitSet(inst.predicates.ground(predicateId, this.args.map(_.cell(context)))))

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
        lhs.update(context, r - exp)
      case ScaleUp =>
        lhs.update(context, r * exp)
      case ScaleDown =>
        lhs.update(context, r / exp)
    }
  }

  /**
   * Returns
   * @param args
   */
  def possibleDelta(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = {
    var sign = rhs.possibleSignWithArgs(EvalContext.onlyLocals(args))
    if(op == Decrease) sign = sign.map(- _)
    else if( (op == ScaleUp || op == ScaleDown) && sign != Some(0)) sign = None // might scale by a fractional amount. sigh.
    val possibleResources = BitSet.empty ++ lhs.possibleGroundings(inst, args)
    sign match {
      case Some(x) if x > 0 => ResourceSummary(addedResources = possibleResources)
      case Some(0) => ResourceSummary.empty
      case Some(x) if x < 0 => ResourceSummary(consumedResources = possibleResources)
      case _ => ResourceSummary(addedResources = possibleResources, consumedResources = possibleResources)
    }
  }
}


case class TimedEffect(time: PDDL.TimeSpecifier, arg: IndexedEffect) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier,  context: EvalContext) {
    if(this.time == time)
      arg.updateState(state, time, context)
  }

  /**
   * Returns
   * @param args
   */
  def possibleDelta(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = {
    arg.possibleDelta(inst, args)
  }
}

case class CondEffect(guard: IndexedCondition, arg: IndexedEffect) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier,  context: EvalContext) {
    if(guard.holds(state, context)) {
      arg.updateState(state, time, context)
    }
  }

  /**
   * Returns
   * @param args
   */
  def possibleDelta(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = {
    // assume conditions can be met.
    arg.possibleDelta(inst, args)
  }
}

case class UniversalEffect(objs: IndexedSeq[Array[Int]], arg: IndexedEffect) extends IndexedEffect {
  def updateState(state: State, time: PDDL.TimeSpecifier,  context: EvalContext) {
    for(list <- objs) {
      arg.updateState(state, time, context.addLocals(list))
    }
  }

  /**
   * Returns
   * @param args
   */
  def possibleDelta(inst: ProblemInstance, args: IndexedSeq[Int]): ResourceSummary = {
    var result = ResourceSummary.empty
    for(list <- objs) {
      result ++= arg.possibleDelta(inst, args ++ list)
    }

    result
  }
}

sealed trait AssignOp
case object Assign extends AssignOp
case object ScaleUp extends AssignOp
case object ScaleDown extends AssignOp
case object Increase extends AssignOp
case object Decrease extends AssignOp
