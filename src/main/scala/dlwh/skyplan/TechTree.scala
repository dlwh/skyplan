package dlwh.skyplan

import collection.immutable.{IndexedSeq, BitSet}
import collection.mutable

/**
 * 
 * @author dlwh
 */
trait TechTree {
  def inst: ProblemInstance
  // offsets are into inst.allViableGroundedActions
  /**
   * Returns list of all actions that can lead to providing this resource
   * The basic idea is that doing any of these actions might get you closer to
   * increasing this resource
   *@param r
   * @return
   */
  def actionsProvidingResource(r: Int):java.util.BitSet
  def actionsConsumingResource(r: Int):java.util.BitSet
  def actionsConsumingAxiom(a: Int):java.util.BitSet
  def actionsProvidingAxiom(a: Int):java.util.BitSet

  def relevantActions(state: State, goal: IndexedCondition):Set[Grounded[IndexedAction]] = {
    val missingSummary:ResourceSummary = goal.computeMissingSummary(state)

    (
      missingSummary.addedAxioms.flatMap(actionsProvidingAxiom(_).iterator)
        ++ missingSummary.consumedAxioms.flatMap(actionsConsumingAxiom(_).iterator)
        ++ missingSummary.addedResources.flatMap(actionsProvidingResource(_).iterator)
        ++ missingSummary.consumedResources.flatMap(actionsConsumingResource(_).iterator)
      ).map(inst.allViableGroundedActions)

  }
}

object TechTree {
  def apply(inst: ProblemInstance):TechTree = {
    // which actions provide this resource
    val resourceProviders = Array.fill(inst.valFuns.groundedIndex.size)(new java.util.BitSet)
    // which actions consume this resource
    val resourceConsumers = Array.fill(inst.valFuns.groundedIndex.size)(new java.util.BitSet)
    // which actions provide this axiom
    val axiomProviders = Array.fill(inst.predicates.groundedIndex.size)(new java.util.BitSet)
    // which actions consume this axiom
    val axiomConsumers = Array.fill(inst.predicates.groundedIndex.size)(new java.util.BitSet)


    for( (grounded@Grounded(a, args, _),index) <- inst.allViableGroundedActions.zipWithIndex) {
      val delta = a.effect.possibleDelta(inst, args)
      for(r <- delta.addedResources) {
        resourceProviders(r) += index
      }
      for(r <- delta.consumedResources) {
        resourceConsumers(r) += index
      }
      for(r <- delta.addedAxioms) {
        axiomProviders(r) += index
      }
      for(r <- delta.consumedAxioms) {
        axiomConsumers(r) += index
      }
    }

//    for(i <- 0 until resourceProviders.length) {
//      println(inst.valFuns.groundedIndex.get(i) + " providers: " + resourceProviders(i).map(inst.allViableGroundedActions))
//    }


//    val i = 6
//    println(i + " " + inst.predicates.groundedIndex.get(i) + " providers: " + axiomProviders(i).map(inst.allViableGroundedActions))

//    for(i <- 0 until axiomProviders.length) {
//      println(i + " " + inst.predicates.groundedIndex.get(i) + " providers: " + axiomProviders(i).map(inst.allViableGroundedActions))
//    }

    // figure out who depends on who. Ugh.
    // bitsets are the actions that might depend on having the resource/axiom produced/consumed
    // rp = resourceProvider, etc.
    val rpDependents = Array.fill(inst.valFuns.groundedIndex.size)(collection.mutable.Set[java.util.BitSet]())
    val rcDependents = Array.fill(inst.valFuns.groundedIndex.size)(collection.mutable.Set[java.util.BitSet]())
    val apDependents = Array.fill(inst.predicates.groundedIndex.size)(collection.mutable.Set[java.util.BitSet]())
    val acDependents = Array.fill(inst.predicates.groundedIndex.size)(collection.mutable.Set[java.util.BitSet]())
    // these guys have some dependent
    val propagators =  collection.mutable.Set[(Symbol, Int)]()

    for( (grounded@Grounded(a, args, _), index) <- inst.allViableGroundedActions.zipWithIndex) {
      val needed:ResourceSummary = (a.contcondition.iterator ++ a.precondition).map(_.resourceSummary(inst, args)).foldLeft(ResourceSummary.empty)(_ ++ _)
      val providersItouch = (resourceProviders ++ resourceConsumers ++ axiomProviders ++ axiomConsumers).filter(_ get index)

      // i need resources of this type, so if i provide a resource of another type,
      // I might need to do any task that
      // provides the consumed resource.
      for(r <- needed.addedResources) {
        rpDependents(r) ++= providersItouch
        propagators += ('RP -> r)
      }

      for(r <- needed.consumedResources) {
        rcDependents(r) ++= providersItouch
        propagators += ('RC -> r)
      }

      for(r <- needed.addedAxioms) {
        apDependents(r) ++= providersItouch
        propagators += ('AP -> r)
      }

      for(r <- needed.consumedAxioms) {
        acDependents(r) ++= providersItouch
        propagators += ('AC -> r)
      }
    }

    val actualPropagators = {
      for( (kind, r) <- propagators.iterator) yield kind match {
        case 'RP => resourceProviders(r) -> rpDependents(r)
        case 'RC => resourceConsumers(r) -> rcDependents(r)
        case 'AP => axiomProviders(r) -> apDependents(r)
        case 'AC => axiomConsumers(r) -> acDependents(r)
        case _ => sys.error("Shouldn't be here.")
      }
    }.toIndexedSeq

    // ok, now we just iterate on the graph until we hit fix point
    var changed = true
    while(changed) {
      val oldSizes = actualPropagators.iterator.map(_._1).foldLeft(0)(_ + _.size)

      for( (src, dests) <- actualPropagators; dest <- dests) {
        dest |= src
      }

      val newSizes = actualPropagators.iterator.map(_._1).foldLeft(0)(_ + _.size)
      changed = oldSizes != newSizes
    }


    val _inst = inst

    val frp = resourceProviders
    val frc = resourceConsumers
    val fap = axiomProviders
    val fac = axiomConsumers


//    println(i + " " + inst.predicates.groundedIndex.get(i) + " providers: " + fap(i).map(inst.allViableGroundedActions))


    new TechTree {
      def inst: ProblemInstance = _inst

      def actionsProvidingResource(r: Int)= frp(r)

      def actionsConsumingResource(r: Int)= frc(r)

      def actionsProvidingAxiom(a: Int)= fap(a)
      def actionsConsumingAxiom(a: Int)= fac(a)
    }

  }
}



/**
 * Summarizes what an action might provide if you execute it. Assumes all conditionals are met.
 * @param addedResources
 * @param consumedResources
 * @param addedAxioms
 * @param consumedAxioms
 */
case class ResourceSummary(addedResources: BitSet = BitSet.empty,
                         consumedResources: BitSet = BitSet.empty,
                         addedAxioms: BitSet = BitSet.empty,
                         consumedAxioms: BitSet = BitSet.empty) {
  def decode(instance: ProblemInstance): String = (
    s"ResourceSummary(addedResources=${addedResources.toIndexedSeq.map(instance.valFuns.groundedIndex.get(_))}, "
    + s"consumedResources=${consumedResources.toIndexedSeq.map(instance.valFuns.groundedIndex.get(_))}, "
    + s"addedAxioms=${addedAxioms.toIndexedSeq.map(instance.predicates.groundedIndex.get(_))},"
    + s"consumedAxioms=${consumedAxioms.toIndexedSeq.map(instance.predicates.groundedIndex.get(_))})"
  )

  def flip: ResourceSummary = ResourceSummary(consumedResources, addedResources, addedAxioms, consumedAxioms)

  def ++(delta: ResourceSummary) = {
    ResourceSummary(addedResources ++ delta.addedResources,
      consumedResources ++ delta.consumedResources,
      addedAxioms ++ delta.addedAxioms,
      consumedAxioms ++ delta.consumedAxioms
    )
  }
}

object ResourceSummary {
  def empty = ResourceSummary(BitSet.empty, BitSet.empty, BitSet.empty, BitSet.empty)
}