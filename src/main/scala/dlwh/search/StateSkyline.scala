package dlwh.search

import dlwh.skyplan.{GoodAxiom, ProblemInstance, State}
import collection.mutable.ArrayBuffer
import java.util
import scala.collection.JavaConverters._
import collection.mutable
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

  def accepts(t: State, cost: Double): Boolean = {
    val candidates = {byCost.headMap(cost, true).values().iterator}.asScala.foldLeft(new util.BitSet()){ (a,b) => a or b; a}
    // determine which sets are on
    for(i <- t.axioms) {
      if(check.axiomOrders(i) == GoodAxiom) {
        candidates.and(axiomSets(i))
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



  }
}

object StateSkyline {

  def factory(inst: ProblemInstance): Factory[State] = new Factory[State] {
    def make: Oracle[State] = new StateSkyline(inst)
  }

}
