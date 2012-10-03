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

case class State(problem: ProblemInstance,
                 time: Double,
                 /** resources are grounded fluents with number domains */
                 resources: HashVector[Double],
                 /** axioms are grounded predicates */
                 var axioms: mutable.BitSet,
                 /** bindings are for object-valued fluents and constants */
                 bindings: OpenAddressHashArray[Int],
                 pendingActions: mutable.PriorityQueue[GroundedAction] = mutable.PriorityQueue.empty[GroundedAction]) {
  lazy val possibleActions = {
    problem.actions.flatMap(_.allPossibleGrounded(this))
  }


  def copy: State = {
    State(problem, time, resources.copy, axioms.clone(), bindings.copy, pendingActions.clone())
  }


  def makeContext(locals: IndexedSeq[Int] = IndexedSeq.empty): EvalContext = new EvalContext {
    def local(i: Int): Int = locals(i)

    def resource(fn: Int, args: IndexedSeq[Int]): Double = {
      resources(problem.refFuns.ground(fn, args))
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

    sb.toString
  }
}

/**
 * 
 * @author dlwh
 */
case class ProblemInstance(objects: GroundedObjects,
                           predicates: Grounding,
                           actions: IndexedSeq[IndexedAction],
                           refFuns: Grounding,
                           valFuns: Grounding,
                           metricExp: ValExpression,
                           initEffect: IndexedEffect) {

  def metric(s: State) = {
    metricExp.resource(s.makeContext(IndexedSeq()))
  }

  def initialState: State = {
    val s = State(this, 0, HashVector.zeros[Double](refFuns.size max 1), mutable.BitSet(), new OpenAddressHashArray[Int](valFuns.size max 1, -1))
    initEffect.updateState(s, s.makeContext())
    s
  }


}


case class GroundedObjects(types: Index[String], index: Index[String], instancesByType: Array[BitSet])

case class Grounding(index: Index[String],
                     groundedByName: Index[String],
                     // propIndex -> args.foldLeft(0)(_ * objectIndex.size + _) -> groundedIndex
                     groundings: Array[Array[Int]],
                     objects: Index[String]) {

  def size = groundedByName.size

  def ground(predicate: Int, args: IndexedSeq[Int]) = {
    if(isAtomic(predicate)) {
      groundings(predicate)(0)
    } else {
      groundings(predicate)(args.foldLeft(0)(_ * objects.size + _))
    }
  }
  def isAtomic(predicate: Int) = groundings(predicate).length == 1
}



object ProblemInstance {


  def indexActions(map: Map[String, Action], objs: GroundedObjects, props: Grounding, resources: Grounding, vars: Grounding) = {
    for( (name, a) <- map.toIndexedSeq) yield {
      IndexedAction.fromAction(a, objs, props, resources, vars)


    }
  }



  def fromPDDL(domain: Domain,
               problem: Problem) = {
    // "object" is common root type.
    val types = Index[String](Iterator("object", "number") ++ domain.types.flatMap(t => Iterator(t.name,t.parent)))
    val objects = Index[String](problem.objects.map(_.name))
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

    new ProblemInstance(objs, propositions, actions, vars, resources, metric, init)

  }


  def groundPropositions(predicate: Map[String, Predicate],
                        groundedObjects: GroundedObjects) = {
    import groundedObjects._
    val predicateIndex = Index[String](predicate.keys)
    val groundedByName = Index[String]()

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
        }
      } else {
        val ind = groundedByName.index(p.name)
        groundings(i) = Array(ind)
      }
    }

    new Grounding(predicateIndex, groundedByName, groundings, index)
  }


  def groundFluents(functions: Map[String, PDDL.Function], objs: GroundedObjects) = {
    val numericIndex = Index[String]()
    val variableIndex = Index[String]()
    val grndNumByName = Index[String]()
    val grndVarByName = Index[String]()
    val groundingsN = new ArrayBuffer[Array[Int]]()
    val groundingsV = new ArrayBuffer[Array[Int]]()

    for( (name, f) <- functions) {
      f.resultType match {
        case "number" =>
          val fi = numericIndex.index(name)
          groundingsN(fi) = groundFluent(objs, f, grndNumByName)
        case _ =>
          val fi = variableIndex.index(name)
          groundingsV(fi) = groundFluent(objs, f, grndVarByName)

      }

    }

    val nums = Grounding(numericIndex, grndNumByName, groundingsN.toArray, objs.index)
    val vars = Grounding(numericIndex, grndNumByName, groundingsN.toArray, objs.index)

    (nums, vars)
  }


  def groundFluent(objs: GroundedObjects, f: PDDL.Function, groundedIndex: MutableIndex[String]): Array[Int] = {
    import objs._
    val arr = Array.fill[Int](index.size * f.args.length)(-1)
    val intArgs = f.args.foldLeft(IndexedSeq(IndexedSeq.empty[Int])) {
      (acc, nextArg) =>
        for (soFar <- acc; a <- instancesByType(types(nextArg.tpe))) yield soFar :+ a
    }

    for (instance <- intArgs) {
      val key = instance.foldLeft(0)(_ * index.size + _)
      assert(key >= 0, "Too many objects.... gonna have to rethink your indexing...")
      val groundedName = instance.map(index.get(_)).mkString("(" + f.name + " " , " ", ")")
      val ind = groundedIndex.index(groundedName)

      arr(key) = ind
    }
    arr
  }

  def populateTypeSets(problem: PDDL.Problem, domain: PDDL.Domain, types: Index[String], objects: Index[String]): Array[BitSet] = {

    val arr = Encoder.fromIndex(types).fillArray(new mutable.BitSet)
    for (o <- problem.objects) {
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

