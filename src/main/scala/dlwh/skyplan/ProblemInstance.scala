package dlwh.skyplan
import breeze.linalg._
import breeze.util._
import collection.immutable.{Queue, BitSet}
import breeze.collection.mutable.OpenAddressHashArray
import collection.mutable
import dlwh.skyplan.PDDL._
import collection.mutable.ArrayBuffer
import dlwh.skyplan.PDDL.Problem
import dlwh.skyplan.PDDL.Domain
import dlwh.skyplan.PDDL.Action
import dlwh.skyplan.PDDL.Predicate
import dlwh.skyplan.Expression.Resource
import breeze.linalg.Axis._0

case class State(problem: ProblemInstance,
                 var time: Double,
                 /** resources are grounded fluents with number domains */
                 resources: HashVector[Double],
                 /** axioms are grounded predicates */
                 var axioms: mutable.BitSet,
                 pendingActions: ActionQueue,
                 actionsWithConditions: ActionQueue) {


  override def hashCode = {
    (time,axioms,resources).hashCode
  }

  override def equals(other: Any) = other match {
    case other: State => other.time == time && resources == other.resources && axioms.subsetOf(other.axioms) && other.axioms.subsetOf(axioms) && pendingActions == other.pendingActions
    case x => false
  }


  def allPossibleGrounded(action: IndexedAction):IndexedSeq[Array[Int]] = {
    val argLists = problem.allViableArgumentListsForAction(action)
    argLists.filter(action.canExecute(this, _))
  }

  def relevantActions: Set[Grounded[IndexedAction]] = relevantActions(problem.goal)

  def relevantActions(goal: IndexedCondition=problem.goal): Set[Grounded[IndexedAction]] = {
    val r = problem.techTree.relevantActions(this, goal).filter { case grounding => grounding.t.canExecute(this, grounding.args)}
//    println("relevant" + problem.techTree.relevantActions(this, problem.goal))
//    println("relevant and doable" + r)
//    println("possible" + problem.allViableGroundedActions.filter(grounding => grounding.t.canExecute(this, grounding.args)))
    r
  }

  def possibleActions = {
    (for( a <- problem.actions.index; argList <- allPossibleGrounded(a)) yield Grounded(a, argList, IndexedSeq(""))).toIndexedSeq
  }

  def applyAction(groundedIndex: Int, duration: Double) {
    val action = problem.allViableGroundedActions(groundedIndex)
    assert(action.t.canExecute(this, action.args), action + " " + this)
    action.t.effect.updateState(this, PDDL.Start, this.makeContext(action.args))
    if(duration > 0) {
      pendingActions.enqueue(groundedIndex, time+duration)
      if (action.t.contcondition.isDefined) actionsWithConditions.enqueue(groundedIndex, time+duration)
    }
  }

  def canElapseTime(delta: Double = -1): Boolean = {
    val targetTime = if (delta > 0) time + delta else (pendingActions.nextTime)
    if (targetTime == Double.PositiveInfinity) return false
    if (actionsWithConditions.nextTime != Double.PositiveInfinity) {
      val s = copy
      while (s.time < targetTime) {
        val newTime = math.min(targetTime, s.pendingActions.nextTime)
        for ((a, t) <- s.pendingActions.dequeue(newTime)) {
          val action = problem.allViableGroundedActions(a)
          // TODO: add duration field to context, use t - time as duration.
          action.t.effect.updateState(s, PDDL.End, this.makeContext(action.args))
        }
        s.actionsWithConditions.activeKeysIterator.foreach(a => {
          if (!s.actionsWithConditions.getTimes(a).isEmpty) {
            val act = problem.allViableGroundedActions(a)
            if (!act.t.contcondition.forall(_.holds(s, s.makeContext(act.args)))) return false
          }
        })
        s.actionsWithConditions.dequeue(newTime)
        s.time = newTime
      }
    }
    true
  }


  def elapseTime(delta: Double = -1) {
    val newTime = if(delta > 0) time + delta else (pendingActions.nextTime)
    for ((a, t) <- pendingActions.dequeue(newTime)) {
      val action = problem.allViableGroundedActions(a)
      // TODO: add duration field to context, use t - time as duration.
      action.t.effect.updateState(this, PDDL.End, this.makeContext(action.args))
    }
    actionsWithConditions.dequeue(newTime)
    time = newTime
  }

  def isEqualToOrDominatedBy(other: State): Boolean = problem.dominanceChecker.isEqualToOrDominatedBy(this, other)

  def copy: State = {
    State(problem, time, resources.copy, axioms.clone(), pendingActions.clone(), actionsWithConditions.clone())
  }


  def makeContext(locals: IndexedSeq[Int] = IndexedSeq.empty): EvalContext = new EvalContext {
    def local(i: Int): Int = {
      locals(i)
    }


    def numLocals: Int = locals.length

    def resource(fn: Int, args: IndexedSeq[Int]): Double = {
      try {
        if (fn == problem.totalTimeIndex) time else
        resources(problem.valFuns.ground(fn, args))
      } catch {
        case e:Exception =>
        throw new RuntimeException("Index Problem " + problem.valFuns.groundings(fn).map(problem.valFuns.groundedIndex.get _) + "\n" + problem.valFuns.index.get(fn) + " " + args.map(problem.objects.index.get _))
      }
    }

    def updateResource(fn: Int, args: IndexedSeq[Int], v: Double) {
      resources(problem.valFuns.ground(fn, args)) = v
    }

    def constResource(fn: Int, args: IndexedSeq[Int]): Double =  {
      problem.constResourceValues(problem.valConstants.ground(fn, args))
    }
  }

  def cost = problem.metric(this)

  override def toString = {
    val sb = new mutable.StringBuilder()
    sb ++= "State("
    sb ++= "cost=" + cost + ",\n "
    sb ++= "time=" + time + ",\n "
    sb ++= "resources=" + Encoder.fromIndex(problem.valFuns.groundedIndex).decode(resources).iterator.filter(_._2 != 0).mkString("[", ", ", "],\n")
    sb ++= "axioms=" + axioms.map(problem.predicates.groundedIndex.get _).toString +",\n"
    sb ++= "pending=" + pendingActions+"\n"
    sb ++= ")"

    sb.toString
  }

  def resource(name: String, args: String*) = {
    import problem._
    val resource = valFuns.ground(valFuns.index(valFuns.index.find(_ == name).get), args.map(objects.index).toIndexedSeq)
    resources(resource)
  }


  def hasAxiom(name: String, args: String*): Boolean = {
    import problem._
    val resource = predicates.ground(predicates.index(predicates.index.find(_ == name).get), args.map(objects.index).toIndexedSeq)
    axioms(resource)
  }
}

/**
 * 
 * @author dlwh
 */
case class ProblemInstance(objects: GroundedObjects,
                           predicates: Grounding[String],
                           constantPredicates: Grounding[String],
                           actions: Grounding[IndexedAction],
                           valFuns: Grounding[String],
                           valConstants: Grounding[String],
                           metricExp: ValExpression,
                           goal: IndexedCondition,
                           initEffect: IndexedEffect,
                           constAxioms: BitSet,
                           constResourceValues: HashVector[Double]) {

  lazy val totalTimeIndex = valFuns.index("total-time")

  def metric(s: State) = {
    metricExp.valueWith(s.makeContext(IndexedSeq()))
  }

  def initialState: State = {
    val s = State(this, 0, HashVector.zeros[Double](valFuns.size max 1), mutable.BitSet(), new ActionQueue(actions), new ActionQueue(actions))
    initEffect.updateState(s, PDDL.Start, s.makeContext())
    s
  }

  def allArgumentLists(action: IndexedAction):IndexedSeq[IndexedSeq[Int]] = {
    val objectSets = action.signature map objects.instancesByType
    val argLists = objectSets.foldLeft(IndexedSeq(IndexedSeq.empty[Int])){ (acc, objs) =>
      for(a <- acc; i <- objs) yield {
        a :+ i
      }
    }

    argLists
  }

  def allGroundedActions = {
    (for( a <- actions.index; argList <- allArgumentLists(a)) yield a -> argList).toIndexedSeq
  }

  // DEBUG METHOD
  def groundedAction(name: String, args: String*) = {
    actions.ground(actions.index(actions.index.find(_.name== name).get), args.map(objects.index).toIndexedSeq)
  }

  val allViableGroundedActions = actions.groundedIndex.toIndexedSeq

  def allViableArgumentListsForAction(action: IndexedAction):IndexedSeq[Array[Int]] = {
    val objects = action.signature map this.objects.instancesByType
    Util.allArgumentListsForChoices(objects)
  }

  lazy val techTree = TechTree(this)

  lazy val dominanceChecker = DominanceChecker(this)

  def deleteFreeVersion = DeleteFree.convertProblemInstance(this)
}


case class GroundedObjects(types: Index[String], index: Index[String], instancesByType: Array[BitSet]) {
  def allGroundingsOfArgumentTypes(seq: IndexedSeq[String]) = {
    val tpes = seq.map(types).map(instancesByType)
    Util.allArgumentListsForChoices(tpes)
  }
}

case class Grounded[T](t: T, args: Array[Int], unindexedArgs: IndexedSeq[String]) {
  override def toString = unindexedArgs.mkString("(" + t + " " , " ", ")")
  override val hashCode = t.hashCode * 17 + java.util.Arrays.hashCode(args)

  override def equals(other: Any): Boolean = other match {
    case Grounded(u,args2, _) => u == t && java.util.Arrays.equals(args,args2)
    case _ => false
  }
}

case class Grounding[T](index: Index[T],
                     ungrounded: Array[Int],
                     groundedIndex: Index[Grounded[T]],
                     // propIndex -> args.foldLeft(0)(_ * objectIndex.size + _) -> groundedIndex
                     groundings: Array[Array[Int]],
                     objects: Index[String]) {

  require(groundedIndex.forall(g => index.contains(g.t) || (throw new RuntimeException(g.toString + " " + index))))

  def size = groundedIndex.size

  def ground(predicate: Int, args: IndexedSeq[Int]):Int = {
    if(isAtomic(predicate)) {
      groundings(predicate)(0)
    } else {
      groundings(predicate)(args.foldLeft(0)(_ * objects.size + _))
    }
  }

  def ground(predicate: T, args: IndexedSeq[Int]):Int = {
    val ind = index(predicate)
    ground(ind, args)
  }

  def unground(groundedIndex: Int): T = {
    index.get(ungrounded(groundedIndex))
  }

  def isAtomic(functionOrPredicate: Int) = groundings(functionOrPredicate).length == 1


}


object ProblemInstance {



  def fromPDDL(domain: Domain,
               problem: Problem) = {
    // "object" is common root type.
    val types = Index[String](Iterator("object", "number") ++ domain.types.flatMap(t => Iterator(t.name,t.parent)))
    val objects = Index[String](domain.constants.map(_.name) ++ problem.objects.map(_.name))
    // Type -> (objects that are that type)
    val instancesByType: Array[BitSet] = populateTypeSets(problem, domain, types, objects)
    val objs = GroundedObjects(types, objects, instancesByType)

    val (preds, constPreds) = partitionConstantPredicates(problem, domain)
    val (functions, constFunctions) = partitionConstantResources(problem, domain)

    val propositions = groundPropositions(preds, objs)
    val constantPropositions = groundPropositions(constPreds, objs)
    val resources = groundFluents(functions, objs)
    val constantResources = groundFluents(constFunctions, objs)
    val actions = indexActions(domain.actions, objs, propositions, constantPropositions, resources, constantResources)

    val metric = problem.metric.map{ case PDDL.MetricSpec(dir, exp) =>
      val base = Expression.fromValExp(exp, resources.index, constantResources.index, Index[String](), objs.index)
      if(dir == PDDL.Maximize)
        Expression.Negation(base)
      else
        base
    }.getOrElse(Expression.Number(0))

    val init = IndexedEffect.fromEffect(problem.initialState, Index[String](), objs,  resources, constantResources, propositions, constantPropositions, ignoreSettingConstants = true)
    val (constAxioms, constResourceValues) = IndexedEffect.getConstantValues(problem.initialState, Index[String], objs, resources, constantResources, propositions, constantPropositions)
    val goal = IndexedCondition.fromCondition(problem.goal, propositions, constantPropositions, resources.index, constantResources.index, Index[String](), objs.index)._1.get

    new ProblemInstance(objs, propositions, constantPropositions, actions, resources, constantResources, metric, goal, init, constAxioms, constResourceValues)

  }

  // Returns (predicates that might change, predicates that will definitely not change)
  def partitionConstantPredicates(problem: Problem, domain: Domain) = {
    val mutablePredicates = domain.actions.values.flatMap { action =>
      action.effect.iterator.flatMap(getModifiedPredicates _)
    }.toSet

    domain.predicates.partition(kv => mutablePredicates(kv._1))
  }

  // Returns (resources that might change, resources that will definitely not change)
  def partitionConstantResources(problem: Problem, domain: Domain) = {
    val mutableResources = domain.actions.values.flatMap { action =>
      action.effect.iterator.flatMap(getModifiedResources _)
    }.toSet

    domain.functions.partition(kv => mutableResources(kv._1))
  }


  import PDDL._
  private def getModifiedPredicates(effect: Effect):Set[String] = effect match {
    case PDDL.AndEffect(conjuncts) => conjuncts.map(getModifiedPredicates _).reduceLeft(_ ++ _)
    case UniversalEffect(_, eff) => getModifiedPredicates(eff)
    case TimedEffect(_, eff) => getModifiedPredicates(eff)
    case EnablePred(p) => Set(p.predicate)
    case DisablePred(p) => Set(p.predicate)
    case AssignEffect(_, _, _) => Set.empty
    case CondEffect(cond, eff) => getModifiedPredicates(eff)
  }

  private def getModifiedResources(effect: Effect):Set[String] = effect match {
    case AndEffect(conjuncts) => conjuncts.map(getModifiedResources _).reduceLeft(_ ++ _)
    case UniversalEffect(_, eff) => getModifiedResources(eff)
    case TimedEffect(_, eff) => getModifiedResources(eff)
    case EnablePred(p) => Set.empty
    case DisablePred(p) => Set.empty
    case AssignEffect(_, lhs, _) => Set(lhs.name)
    case CondEffect(cond, eff) => getModifiedResources(eff)
  }


  def indexActions(map: Map[String, Action],
                   objs: GroundedObjects,
                   props: Grounding[String],
                   constProps: Grounding[String],
                   resources: Grounding[String],
                   constResources: Grounding[String]) = {
    import objs._
    val indexed = for( (name, a) <- map.toIndexedSeq) yield {
      IndexedAction.fromAction(a, Index[String](), objs, props, constProps, resources, constResources)
    }


    val predicateIndex = Index[IndexedAction](indexed)
    val groundedByName = Index[Grounded[IndexedAction]]()
    val inverse = ArrayBuffer[Int]()

    val groundings = new Array[Array[Int]](predicateIndex.size)
    for( (a, i) <- predicateIndex.pairs) {
      if(a.signature.nonEmpty) {
        val intArgs = Util.allArgumentListsForChoices(a.signature.map(instancesByType))
        groundings(i) = new Array[Int](math.pow(index.size, a.signature.length).toInt max 1)
        java.util.Arrays.fill(groundings(i),-1)

        for(instance <- intArgs) {
          val key = instance.foldLeft(0)(_ * index.size + _)
          assert(key >= 0, "Too many objects.... gonna have to rethink your indexing...")
          val ind = groundedByName.index(Grounded(a, instance, instance.map(objs.index.get _)))

          groundings(i)(key) = ind
          inverse += i
        }
      } else {
        val ind = groundedByName.index(Grounded(a, Array.empty, IndexedSeq.empty))
        groundings(i) = Array(ind)
      }
    }

    new Grounding(predicateIndex, inverse.toArray, groundedByName, groundings, index)

  }


  def groundPropositions(predicate: Map[String, Predicate],
                        groundedObjects: GroundedObjects) = {
    import groundedObjects._
    val predicateIndex = Index[String](predicate.keys)
    val groundedByName = Index[Grounded[String]]()
    val inverse = ArrayBuffer[Int]()

    val groundings = new Array[Array[Int]](predicateIndex.size)
    for( (pn, i) <- predicateIndex.pairs; p =  predicate(pn)) {
      if(p.args.nonEmpty) {
        val intArgs = p.args.foldLeft(IndexedSeq(IndexedSeq.empty[Int])) { (acc, nextArg) =>
          for( soFar <- acc; a <- instancesByType(types(nextArg.tpe))) yield soFar :+ a
        }
        groundings(i) = Array.fill[Int](math.pow(index.size, p.args.length).toInt max 1)(-1)

        for(instance <- intArgs) {
          val key = instance.foldLeft(0)(_ * index.size + _)
          assert(key >= 0, "Too many objects.... gonna have to rethink your indexing...")
          val ind = groundedByName.index(Grounded(pn, instance.toArray, instance.map(index.get _)))

          groundings(i)(key) = ind
          inverse += i
        }
      } else {
        val ind = groundedByName.index(Grounded(p.name, Array.empty, IndexedSeq.empty))
        groundings(i) = Array(ind)
      }
    }

    new Grounding(predicateIndex, inverse.toArray, groundedByName, groundings, index)
  }


  def groundFluents(functions: Map[String, PDDL.Function], objs: GroundedObjects) = {
    val numericIndex = Index[String]()
    val grndNumByName = Index[Grounded[String]]()
    val groundingsN = new ArrayBuffer[Array[Int]]()
    val inverseN = ArrayBuffer[Int]()

    // Special placeholder value for time spent
    numericIndex.index("total-time")
    groundingsN += groundFluent(objs, PDDL.Function("total-time", IndexedSeq.empty), grndNumByName)
    inverseN ++= Array.fill(groundingsN.last.length)(groundingsN.length - 1)

    for( (name, f) <- functions) {
      f.resultType match {
        case "number" =>
          val fi = numericIndex.index(name)
          groundingsN += groundFluent(objs, f, grndNumByName)
          inverseN ++= Array.fill(groundingsN.last.length)(groundingsN.length - 1)
        case _ =>
     }

    }

    val nums = Grounding(numericIndex, inverseN.toArray, grndNumByName, groundingsN.toArray, objs.index)

    nums
  }


  def groundFluent(objs: GroundedObjects, f: PDDL.Function, groundedIndex: MutableIndex[Grounded[String]]): Array[Int] = {
    import objs._
    val arr = Array.fill[Int](math.pow(index.size,f.args.length).toInt max 1)(-3)
    val intArgs = objs.allGroundingsOfArgumentTypes(f.args.map(_.tpe))


    for (instance <- intArgs) {
      val key = instance.foldLeft(0)(_ * index.size + _)
      assert(key >= 0, "Too many objects.... gonna have to rethink your indexing...")
      val ind = groundedIndex.index(Grounded(f.name, instance, instance.map(index.get _)))
      assert(ind != -1)

      arr(key) = ind
    }
    arr
  }

  def populateTypeSets(problem: PDDL.Problem, domain: PDDL.Domain, types: Index[String], objects: Index[String]): Array[BitSet] = {

    val arr = Encoder.fromIndex(types).fillArray(new mutable.BitSet)
    for (o <- problem.objects ++ domain.constants) {
      val i = objects(o.name)
      val t = types(o.tpe)
      assert(t > -1, types + " " + o.tpe)
      arr(t) += i
      arr(0) += i
    }

    val parentTypeIndices = Array.fill(types.size)(0)
    for (tpe <- domain.types) {
      parentTypeIndices(types(tpe.name)) = types(tpe.parent)
    }

    // populate inheritance directly. not the most
    // efficient thing. meh.
    val changed = Array.fill(arr.length)(true)
    var someChanged = true
    while (someChanged) {
      var t = arr.length - 1
      someChanged = false
      while (t > 0) {
        if (changed(t)) {
          arr(parentTypeIndices(t)) |= arr(t)
          changed(t) = false
          changed(parentTypeIndices(t)) = true
          someChanged = true
        }
        t -= 1
      }

    }
    arr.map(BitSet.empty ++ _)

  }
}

