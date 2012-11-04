package dlwh.skyplan

import collection.immutable.BitSet

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
  def actionsProvidingResource(r: Int):BitSet
  def actionsConsumingResource(r: Int):BitSet
  def actionsConsumingAxiom(a: Int):BitSet
  def actionsProvidingAxiom(a: Int):BitSet

  def relevantActions(state: State, goal: IndexedCondition):Set[Grounded[IndexedAction]] = {
    val missingSummary:ResourceSummary = goal.computeMissingSummary(state)

    (
      missingSummary.addedAxioms.flatMap(actionsProvidingAxiom(_))
        ++ missingSummary.consumedAxioms.flatMap(actionsConsumingAxiom(_))
        ++ missingSummary.addedResources.flatMap(actionsProvidingResource(_))
        ++ missingSummary.consumedResources.flatMap(actionsConsumingResource(_))
      ).map(inst.allViableGroundedActions)

  }
}

object TechTree {
  def apply(inst: ProblemInstance):TechTree = {
    val resourceProviders = Array.fill(inst.valFuns.groundedIndex.size)(new collection.mutable.BitSet)
    val resourceConsumers = Array.fill(inst.valFuns.groundedIndex.size)(new collection.mutable.BitSet)
    val axiomProviders = Array.fill(inst.predicates.groundedIndex.size)(new collection.mutable.BitSet)
    val axiomConsumers = Array.fill(inst.predicates.groundedIndex.size)(new collection.mutable.BitSet)


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


//    for(i <- 0 until axiomProviders.length) {
//      println(inst.predicates.groundedIndex.get(i) + " providers: " + axiomProviders(i).map(inst.allViableGroundedActions))
//    }

    // figure out who depends on who. Ugh.
    // rp = resourceProvider, etc.
    val rpDependents = Array.fill(inst.valFuns.groundedIndex.size)(collection.mutable.Set[collection.mutable.BitSet]())
    val rcDependents = Array.fill(inst.valFuns.groundedIndex.size)(collection.mutable.Set[collection.mutable.BitSet]())
    val apDependents = Array.fill(inst.predicates.groundedIndex.size)(collection.mutable.Set[collection.mutable.BitSet]())
    val acDependents = Array.fill(inst.predicates.groundedIndex.size)(collection.mutable.Set[collection.mutable.BitSet]())
    // these guys have some dependent
    val propagators =  collection.mutable.Set[(Symbol, Int)]()

    for( (grounded@Grounded(a, args, _), index) <- inst.allViableGroundedActions.zipWithIndex) {
      val needed:ResourceSummary = a.precondition.map(_.resourceSummary(inst, args)).getOrElse(ResourceSummary.empty)
      val providersItouch = (resourceProviders ++ resourceConsumers ++ axiomProviders ++ axiomConsumers).filter(_ contains index)

      // i need resources of this type, so if i provide a resource of another type,
      // I might need to do any task that
      // provides the ocnsumed resource.
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
        dest ++= src
      }

      val newSizes = actualPropagators.iterator.map(_._1).foldLeft(0)(_ + _.size)
      changed = oldSizes != newSizes
    }


    val i = inst

    val frp = resourceProviders.map(collection.immutable.BitSet() ++ _)
    val frc = resourceConsumers.map(collection.immutable.BitSet() ++ _)
    val fap = axiomProviders.map(collection.immutable.BitSet() ++ _)
    val fac = axiomConsumers.map(collection.immutable.BitSet() ++ _)

    new TechTree {
      def inst: ProblemInstance = i

      def actionsProvidingResource(r: Int): BitSet = frp(r)

      def actionsConsumingResource(r: Int): BitSet = frc(r)

      def actionsProvidingAxiom(a: Int): BitSet = fap(a)
      def actionsConsumingAxiom(a: Int): BitSet = fac(a)
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
  def flip: ResourceSummary = ResourceSummary(consumedResources, addedResources, addedAxioms, consumedAxioms)

  def ++(delta: ResourceSummary) = {
    ResourceSummary(addedResources ++ delta.addedResources,
      consumedResources ++ delta.consumedResources,
      addedAxioms ++ delta.addedAxioms,
      consumedResources ++ delta.consumedResources
    )
  }
}

object ResourceSummary {
  def empty = ResourceSummary(BitSet.empty, BitSet.empty, BitSet.empty, BitSet.empty)
}