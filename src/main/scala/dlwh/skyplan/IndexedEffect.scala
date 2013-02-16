package dlwh.skyplan

import collection.immutable.BitSet
import dlwh.skyplan.Expression.Resource
import dlwh.skyplan.PDDL.{AssignEffect, Effect}
import breeze.util.Index
import breeze.linalg.Axis._0
import breeze.linalg.HashVector


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
                 resources: Grounding[String],
                 constResources: Grounding[String],
                 preds: Grounding[String],
                 constPreds: Grounding[String],
                 ignoreSettingConstants: Boolean=false):IndexedEffect = {
    def rec(eff: Effect, varBindings: Index[String]):IndexedEffect = eff match {
      case PDDL.AndEffect(xs) =>
        val recs = xs.map(rec(_,varBindings))
        AndEffect(recs)
      case PDDL.DisablePred(pred) =>
        val predIndex = preds.index(pred.predicate)
        if(predIndex < 0)
          if(ignoreSettingConstants)
            NoEffect
          else throw new RuntimeException("Not a dynamic predicate: " + pred)
        else DisableDynamicPredicate(predIndex, pred.args.map(CellExpression.fromRefExp(_, varBindings, objs.index)))
      case PDDL.EnablePred(pred) =>
        val predIndex = preds.index(pred.predicate)
        if(predIndex < 0)
          if(ignoreSettingConstants)
            NoEffect
          else throw new RuntimeException("Not a dynamic predicate: " + pred)
        else EnableDynamicPredicate(predIndex, pred.args.map(CellExpression.fromRefExp(_, varBindings, objs.index)))
      case ae@AssignEffect(op, lhs, rhs) =>
        if(constResources.index(lhs.name) >= 0) {
          if(ignoreSettingConstants)
            NoEffect
          else {
            throw new RuntimeException("Not a dynamic resource: " + lhs)
          }
        } else {
          val lcell = Expression.fromValExp(lhs, resources.index, constResources.index, varBindings, objs.index)
          val rcell = Expression.fromValExp(rhs, resources.index, constResources.index, varBindings, objs.index)
          AssignToResource(op, lcell.asInstanceOf[Expression.Resource],  rcell)
        }
      case PDDL.UniversalEffect(args, eff) =>
        val extended = Index(varBindings ++ args.map(_.name))
        UniversalEffect(objs.allGroundingsOfArgumentTypes(args.map(_.tpe)).map(_.toArray), rec(eff, extended))
      case PDDL.TimedEffect(spec, arg) =>
        val a = rec(arg, varBindings)
        new TimedEffect(spec, a)
      case PDDL.CondEffect(guard, arg) =>
        val ig = IndexedCondition.fromCondition(guard, preds, constPreds, resources.index, constResources.index, varBindings, objs.index)
        val a = rec(arg, varBindings)
        new CondEffect(ig, a)
    }
    rec(effect, locals)
  }

  def getConstantValues(effect: Effect,
                        locals: Index[String],
                        objs: GroundedObjects,
                        resources: Grounding[String],
                        constResources: Grounding[String],
                        preds: Grounding[String],
                        constPreds: Grounding[String]):(BitSet, HashVector[Double]) = {
    val resourceCounts = HashVector.zeros[Double](constResources.size)
    val set = collection.mutable.BitSet.empty
    def rec(eff: Effect, varBindings: Index[String]):Unit = eff match {
      case PDDL.AndEffect(xs) =>
         xs.foreach(rec(_,varBindings))
      case PDDL.EnablePred(pred) =>
        val predIndex = constPreds.index(pred.predicate)
        if (predIndex >= 0) {
          val predExprs = pred.args.map(CellExpression.fromRefExp(_, varBindings, objs.index))
          set += constPreds.ground(predIndex, predExprs.map(_.cell(EvalContext.emptyContext)))
        }
      case ae@AssignEffect(op, lhs, rhs) =>
        val predIndex = constResources.index(lhs.name)
        if(predIndex >= 0) {
          val rcell = Expression.fromValExp(rhs, resources.index, constResources.index, varBindings, objs.index)
          val context = EvalContext.emptyContext
          val predExprs = lhs.args.map(CellExpression.fromRefExp(_, varBindings, objs.index))
          val resource = constResources.ground(predIndex, predExprs.map(_.cell(context)))
          val exp = rcell.valueWith(context)
          op match {
            case Assign =>
              if(exp != 0)
                resourceCounts(resource) = exp
            case Increase =>
              if(exp != 0)
                resourceCounts(resource) += exp
            case Decrease =>
              if(exp != 0)
                resourceCounts(resource) -= exp
            case ScaleUp =>
              if(exp != 1)
                resourceCounts(resource) *= exp
            case ScaleDown =>
              if(exp != 1)
                resourceCounts(resource) /= exp
          }
        }
      case _ =>
        throw new RuntimeException("Not a valid effect for initial effect: " + eff)
    }

    rec(effect, locals)

    BitSet.empty ++ set -> resourceCounts
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
