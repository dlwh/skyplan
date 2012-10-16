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

  def ground(state: State,
             args: IndexedSeq[Int]) = {
    GroundedAction(this, args, state.time + duration.map(_.valueWith(state.makeContext(args))).getOrElse(0.0))
  }

  def allPossibleGrounded(state: State) = {
    val objects = signature map state.problem.objects.instancesByType
    val argLists = objects.foldLeft(IndexedSeq(IndexedSeq.empty[Int])){ (acc, objs) =>
      for(a <- acc; i <- objs) yield {
        a :+ i
      }
    }

    val grounded = ArrayBuffer[GroundedAction]()


    for(list <- argLists) {
      val ga = ground(state, list)
      val holds = ga.canExecute(state)
      if(holds) {
        grounded += ground(state, list)
      }
    }

    grounded.toIndexedSeq
  }


}


object IndexedAction {
  def fromAction(a: PDDL.Action,
                 standardLocals: Index[String],
                 objs: GroundedObjects,
                 props: Grounding,
                 resources: Grounding,
                 vars: Grounding) = {
    val locals = Index(a.args.map(_.name))
    val prec = a.precondition.map(IndexedCondition.fromCondition(_, props, vars.index, resources.index, locals, objs.index))
    val duration = a.duration.map {
      case PDDL.StandardDuration(comp, value) => Expression.fromValExp(value, vars.index, resources.index, locals, objs.index)
    }


    val effect = a.effect.map(IndexedEffect.fromEffect(_, locals, objs, vars, resources, props)).getOrElse(NoEffect)

    new IndexedAction(a.name, a.args.map(a => objs.types(a.tpe)), prec, effect, duration)
  }
}