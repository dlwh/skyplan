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
                 /** bindings are for object-valued fluents and constants */
                 bindings: OpenAddressHashArray[Int],
                 pendingActions: ActionQueue) {

  override def hashCode = {
    (time,axioms).hashCode
  }

  def allPossibleGrounded(action: IndexedAction):IndexedSeq[IndexedSeq[Int]] = {
    val idx = problem.actions.index(action)
    val objects = action.signature map problem.objects.instancesByType
    val argLists = objects.foldLeft(IndexedSeq(IndexedSeq.empty[Int])){ (acc, objs) =>
      for(a <- acc; i <- objs) yield {
        a :+ i
      }
    }

    argLists.filter(action.canExecute(this, _))
  }

  def possibleActions = {
    (for( a <- problem.actions.index; argList <- allPossibleGrounded(a)) yield a -> argList).toIndexedSeq
  }

  def applyAction(groundedIndex: Int, duration: Double) {
    problem.actions.unground(groundedIndex).effect.updateState(this, PDDL.Start, this.makeContext())
    if(duration > 0) {
      pendingActions.enqueue(groundedIndex, time+duration)
    }
  }

  def hasAction() = {
    pendingActions.nextTime != Double.PositiveInfinity
  }

  def elapseTime(delta: Double = -1) {
    val newTime = if(delta > 0) time + delta else (pendingActions.nextTime)
    for ((a, t) <- pendingActions.dequeue()) {
      // TODO: add duration field to context, use t - time as duration.
      problem.actions.unground(a).effect.updateState(this, PDDL.End, this.makeContext())
    }
    time = newTime
  }


  def copy: State = {
    State(problem, time, resources.copy, axioms.clone(), bindings.copy, pendingActions.clone())
  }


  def makeContext(locals: IndexedSeq[Int] = IndexedSeq.empty): EvalContext = new EvalContext {
    def local(i: Int): Int = {
      locals(i)
    }


    def numLocals: Int = locals.length

    def resource(fn: Int, args: IndexedSeq[Int]): Double = {
      try {
        resources(problem.valFuns.ground(fn, args))
      } catch {
        case e:Exception =>
        throw new RuntimeException("Index Problem " + problem.valFuns.groundings(fn).map(problem.valFuns.groundedByName.get _) + "\n" + problem.valFuns.index.get(fn) + " " + args.map(problem.objects.index.get _))
      }
    }

    def cell(fn: Int, args: IndexedSeq[Int]): Int = {
      bindings(problem.refFuns.ground(fn, args))
    }

    def updateResource(fn: Int, args: IndexedSeq[Int], v: Double) {
      resources(problem.valFuns.ground(fn, args)) = v
    }

    def updateCell(fn: Int, args: IndexedSeq[Int], v: Int) {
      bindings(problem.refFuns.ground(fn, args)) = v
    }
  }

  def cost = problem.metric(this)

  override def toString = {
    val sb = new mutable.StringBuilder()
    sb ++= "State("
    sb ++= "time=" + time + ", "
    sb ++= "resources=" + Encoder.fromIndex(problem.refFuns.groundedByName).decode(resources).toString +",\n "
    sb ++= "axioms=" + axioms.map(problem.predicates.groundedByName.get _).toString +",\n"
    sb ++= "pending=" + pendingActions+"\n"
    sb ++= ")"

    sb.toString
  }
}

/**
 * 
 * @author dlwh
 */
case class ProblemInstance(objects: GroundedObjects,
                           predicates: Grounding[String],
                           actions: Grounding[IndexedAction],
                           refFuns: Grounding[String],
                           valFuns: Grounding[String],
                           metricExp: ValExpression,
                           goal: IndexedCondition,
                           initEffect: IndexedEffect) {

  def metric(s: State) = {
    metricExp.valueWith(s.makeContext(IndexedSeq()))
  }

  def initialState: State = {
    val s = State(this, 0, HashVector.zeros[Double](valFuns.size max 1), mutable.BitSet(), new OpenAddressHashArray[Int](refFuns.size max 1, -1), new ActionQueue(actions))
    initEffect.updateState(s, PDDL.Start, s.makeContext())
    s
  }

  // DEBUG METHOD
  def groundedAction(state: State, name: String, args: String*) = {
    actions.ground(actions.index(actions.index.find(_.name== name).get), args.map(objects.index).toIndexedSeq)
  }


}


case class GroundedObjects(types: Index[String], index: Index[String], instancesByType: Array[BitSet]) {
  def allGroundingsOfArgumentTypes(seq: IndexedSeq[String]) = {
    val tpes = seq.map(types).map(instancesByType)
    tpes.foldLeft(IndexedSeq(IndexedSeq.empty[Int])){ (acc, objs) =>
      for(list <- acc; o <- objs) yield list :+ o
    }

  }
}

case class Grounding[T](index: Index[T],
                     ungrounded: Array[Int],
                     groundedByName: Index[String],
                     // propIndex -> args.foldLeft(0)(_ * objectIndex.size + _) -> groundedIndex
                     groundings: Array[Array[Int]],
                     objects: Index[String]) {

  def size = groundedByName.size

  def ground(predicate: Int, args: IndexedSeq[Int]):Int = {
    if(isAtomic(predicate)) {
      groundings(predicate)(0)
    } else {
      groundings(predicate)(args.foldLeft(0)(_ * objects.size + _))
    }
  }

  def ground(predicate: T, args: IndexedSeq[Int]):Int = {
    ground(index(predicate), args)
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

    val propositions = groundPropositions(domain.predicates, objs)
    val (resources, vars) = groundFluents(domain.functions, objs)
    val actions = indexActions(domain.actions, objs, propositions, resources, vars)

    val metric = problem.metric.map{ case PDDL.MetricSpec(dir, exp) =>
      val base = Expression.fromValExp(exp, vars.index, resources.index, Index[String](), objs.index)
      if(dir == PDDL.Maximize)
        Expression.Negation(base)
      else
        base
    }.getOrElse(Expression.Number(0))

    val init = IndexedEffect.fromEffect(problem.initialState, Index[String](), objs, vars, resources, propositions)
    val goal = IndexedCondition.fromCondition(problem.goal, propositions, vars.index, resources.index, Index[String](), objs.index)

    new ProblemInstance(objs, propositions, actions, vars, resources, metric, goal, init)

  }

  def indexActions(map: Map[String, Action], objs: GroundedObjects, props: Grounding[String], resources: Grounding[String], vars: Grounding[String]) = {
    import objs._
    val indexed = for( (name, a) <- map.toIndexedSeq) yield {
      IndexedAction.fromAction(a, Index[String](), objs, props, resources, vars)
    }


    val predicateIndex = Index[IndexedAction](indexed)
    val groundedByName = Index[String]()
    val inverse = ArrayBuffer[Int]()

    val groundings = new Array[Array[Int]](predicateIndex.size)
    for( (a, i) <- predicateIndex.pairs) {
      if(a.signature.nonEmpty) {
        val intArgs = a.signature.foldLeft(IndexedSeq(IndexedSeq.empty[Int])) { (acc, nextArg) =>
          for( soFar <- acc; a <- instancesByType(nextArg)) yield soFar :+ a
        }
        groundings(i) = Array.fill[Int](math.pow(index.size, a.signature.length).toInt max 1)(-1)

        for(instance <- intArgs) {
          val key = instance.foldLeft(0)(_ * index.size + _)
          assert(key >= 0, "Too many objects.... gonna have to rethink your indexing...")
          val groundedName = instance.map(index.get(_)).mkString("(" + a.name + " " , " ", ")")
          val ind = groundedByName.index(groundedName)

          groundings(i)(key) = ind
          inverse += i
        }
      } else {
        val ind = groundedByName.index(a.name)
        groundings(i) = Array(ind)
      }
    }

    new Grounding(predicateIndex, inverse.toArray, groundedByName, groundings, index)

  }


  def groundPropositions(predicate: Map[String, Predicate],
                        groundedObjects: GroundedObjects) = {
    import groundedObjects._
    val predicateIndex = Index[String](predicate.keys)
    val groundedByName = Index[String]()
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
          val groundedName = instance.map(index.get(_)).mkString("(" + p.name + " " , " ", ")")
          val ind = groundedByName.index(groundedName)

          groundings(i)(key) = ind
          inverse += i
        }
      } else {
        val ind = groundedByName.index(p.name)
        groundings(i) = Array(ind)
      }
    }

    new Grounding(predicateIndex, inverse.toArray, groundedByName, groundings, index)
  }


  def groundFluents(functions: Map[String, PDDL.Function], objs: GroundedObjects) = {
    val numericIndex = Index[String]()
    val variableIndex = Index[String]()
    val grndNumByName = Index[String]()
    val grndVarByName = Index[String]()
    val groundingsN = new ArrayBuffer[Array[Int]]()
    val groundingsV = new ArrayBuffer[Array[Int]]()
    val inverseN = ArrayBuffer[Int]()
    val inverseV = ArrayBuffer[Int]()

    for( (name, f) <- functions) {
      f.resultType match {
        case "number" =>
          val fi = numericIndex.index(name)
          groundingsN += groundFluent(objs, f, grndNumByName)
          inverseN ++= Array.fill(groundingsN.last.length)(groundingsN.length - 1)
        case _ =>
          val fi = variableIndex.index(name)
          groundingsV += groundFluent(objs, f, grndVarByName)
          inverseV ++= Array.fill(groundingsV.last.length)(groundingsV.length - 1)
     }

    }

    val nums = Grounding(numericIndex, inverseN.toArray, grndNumByName, groundingsN.toArray, objs.index)
    val vars = Grounding(variableIndex, inverseV.toArray, grndVarByName, groundingsV.toArray, objs.index)

    (nums, vars)
  }


  def groundFluent(objs: GroundedObjects, f: PDDL.Function, groundedIndex: MutableIndex[String]): Array[Int] = {
    import objs._
    val arr = Array.fill[Int](math.pow(index.size,f.args.length).toInt max 1)(-3)
    val intArgs = f.args.foldLeft(IndexedSeq(IndexedSeq.empty[Int])) { (acc, nextArg) =>
        for (soFar <- acc; a <- instancesByType(types(nextArg.tpe))) yield soFar :+ a
    }


    for (instance <- intArgs) {
      val key = instance.foldLeft(0)(_ * index.size + _)
      assert(key >= 0, "Too many objects.... gonna have to rethink your indexing...")
      val groundedName = instance.map(index.get(_)).mkString("(" + f.name + " " , " ", ")")
      val ind = groundedIndex.index(groundedName)
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

