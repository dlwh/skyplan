package dlwh.skyplan

import breeze.util.Index

/**
 * 
 * @author dlwh
 */
object DeleteFree {
  def convertProblemInstance(instance: ProblemInstance): ProblemInstance = {
    instance.copy(actions=makeDeleteFreeActions(instance.actions))
  }

  private def makeDeleteFreeActions(actions: Grounding[IndexedAction]):Grounding[IndexedAction] = {
    val newIndex = Index[IndexedAction]()
    val map = collection.mutable.Map[IndexedAction, IndexedAction]()
    val newGrounded = Index[Grounded[IndexedAction]]

    for(a <- actions.index) {
      val deleteFree = convertAction(a)
      newIndex.index(deleteFree)
      map += (a -> deleteFree)
    }

    for (a <- actions.groundedIndex) {
      newGrounded.index(Grounded(map(a.t), a.args, a.unindexedArgs))
    }
//    assert(newIndex.toString == actions.index.toString, newIndex + " " + actions.index)
//    assert(newGrounded.toString == actions.groundedIndex.toString, newGrounded + " " + actions.groundedIndex)

    actions.copy(newIndex,groundedIndex=newGrounded)
  }

  private def convertAction(a: IndexedAction) = {
    a.copy(precondition=a.precondition.map(convertCondition _), effect=convertEffect(a.effect))
  }

  private def convertEffect(e: IndexedEffect):IndexedEffect = e match {
    case e:AndEffect => AndEffect(e.conjuncts.map(convertEffect))
    case e:CondEffect => CondEffect(convertCondition(e.guard), convertEffect(e))
    case e:DisableDynamicPredicate => NoEffect
    case e:DisableMultiple => NoEffect
    case e:EnableMultiple => e
    case e:EnableDynamicPredicate => e
    case NoEffect => e
    case TimedEffect(when, e2) => TimedEffect(when, convertEffect(e2))
    case UniversalEffect(when, e2) => UniversalEffect(when, convertEffect(e2))
    case e@AssignToResource(Increase, lhs, Expression.Number(x)) if x >= 0 => e
    case e@AssignToResource(Decrease, lhs, Expression.Number(x)) if x <= 0 => e
    case e@AssignToResource(Increase, lhs, Expression.Number(x)) => NoEffect
    case e@AssignToResource(Decrease, lhs, Expression.Number(x)) => NoEffect
    case e@AssignToResource(Assign, lhs, Expression.Number(x)) => e // TODO: hrm.
    case e: AssignToResource => sys.error("..." + e)
  }


  private def convertCondition(e: IndexedCondition): IndexedCondition = e match {
    case e: AndCondition => AndCondition(e.conjuncts.map(convertCondition))
    case e: OrCondition => OrCondition(e.disjuncts.map(convertCondition))
    case e@ BinaryCompCondition(PDDL.<=, lhs, Expression.Number(x)) => TrueCondition
    case e@ BinaryCompCondition(PDDL.<, lhs, Expression.Number(x)) => TrueCondition
    case e@ BinaryCompCondition(PDDL.>=, lhs, Expression.Number(x)) => e
    case e@ BinaryCompCondition(PDDL.>, lhs, Expression.Number(x)) => e
    case e: BinaryCompCondition => sys.error("Can't deal." + " " + e)
    case e: NotCondition => TrueCondition
    case TrueCondition => TrueCondition
    case e: PredicateCondition => e
    case e: ConstPredicateCondition => e
  }

}
