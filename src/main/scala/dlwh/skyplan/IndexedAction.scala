package dlwh.skyplan

import breeze.util.Index
import collection.mutable.ArrayBuffer
import javax.xml.datatype.Duration

case class IndexedAction(name: String,
                         // args by type.
                         signature: IndexedSeq[Int],
                         precondition: Option[IndexedCondition],
                         effect: IndexedEffect,
                         duration: Option[ValExpression]) {

  def canExecute(state: State, args: IndexedSeq[Int]) = {
    precondition.forall(_.holds(state, state.makeContext(args)))
  }

  def durationOf(state: State, args: IndexedSeq[Int]) = {
    duration.map(_.valueWith(state.makeContext(args))).getOrElse(0.0)
  }

}


object IndexedAction {
  def fromAction(a: PDDL.Action,
                 standardLocals: Index[String],
                 objs: GroundedObjects,
                 props: Grounding[String],
                 resources: Grounding[String],
                 vars: Grounding[String]) = {
    val locals = Index(a.args.map(_.name))
    val prec = a.precondition.map(IndexedCondition.fromCondition(_, props, vars.index, resources.index, locals, objs.index))
    val duration = a.duration.map {
      case PDDL.StandardDuration(comp, value) => Expression.fromValExp(value, vars.index, resources.index, locals, objs.index)
    }


    val effect = a.effect.map(IndexedEffect.fromEffect(_, locals, objs, vars, resources, props)).getOrElse(NoEffect)

    new IndexedAction(a.name, a.args.map(a => objs.types(a.tpe)), prec, effect, duration)
  }
}