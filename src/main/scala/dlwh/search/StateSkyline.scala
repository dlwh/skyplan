package dlwh.search

import dlwh.skyplan._
import collection.mutable.ArrayBuffer
import java.util
import scala.collection.JavaConverters._
import dlwh.search.Oracle.Factory

/**
 * 
 * @author dlwh
 */
class StateSkyline(instance: ProblemInstance) extends Oracle[State] {
  val check = instance.dominanceChecker
  val states = ArrayBuffer[State]()
  def size: Int = states.size

  val byCost = new util.TreeMap[Double, util.BitSet]
  val axiomSets = Array.fill(instance.predicates.size)(new util.BitSet())
  val resourceSets = Array.fill(instance.valFuns.size)(new util.BitSet())

  def accepts(t: State, cost: Double): Boolean = {
    val candidates = {byCost.headMap(cost, true).values().iterator}.asScala.foldLeft(new util.BitSet()){ (a,b) => a or b; a}
    // determine which sets are on
    for(i <- t.axioms) {
      if(check.axiomOrders(i) == GoodAxiom) {
        candidates.and(axiomSets(i))
      }
    }

    if(candidates.nextSetBit(0) >= 0)
      for(i <- t.resources.activeKeysIterator) {
        if(check.resourceOrders(i) == MoreIsBetter) {
          if(t.resources(i) != 0.0)
            candidates.and(resourceSets(i))
        }
      }

    var i = candidates.nextSetBit(0)
    while(i >= 0) {
      if(check.isDominatedBy(t, states(i)) ) {
        return false
      }
      i = candidates.nextSetBit(i+1)
    }

    actuallyAdd(t, cost)
    true
  }

  def actuallyAdd(t: State, cost: Double) {
    val ind = states.length
    states += t
    val costSet = byCost.get(cost)
    if(costSet == null) {
      byCost.put(cost, new util.BitSet(ind))
    } else {
      costSet.set(ind)
    }

    for(i <- t.axioms) {
      if(check.axiomOrders(i) == GoodAxiom) {
        axiomSets(i).set(ind)
      }
    }

    for(i <- t.resources.activeKeysIterator) {
      if(check.resourceOrders(i) == MoreIsBetter) {
        if(t.resources(i) != 0.0)
          resourceSets(i).set(ind)
      }
    }



  }
}

object StateSkyline {

  def factory(inst: ProblemInstance): Factory[State] = new Factory[State] {
    def make: Oracle[State] = new StateSkyline(inst)
  }

}
