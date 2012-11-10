package dlwh.search

import java.util
import dlwh.search.Oracle.Factory

class NaiveSkyline[T:PartialOrdering] extends Oracle[T] {
  private val order = implicitly[PartialOrdering[T]]
  var size = 0
  private val set = new java.util.TreeMap[Double, util.ArrayList[T]]()


  def accepts(state: T, cost: Double): Boolean = {
    val headSet = set.headMap(cost, true)
    // guys closer in cost are more likely to dominate than guys lower in cost.
    val it = headSet.descendingMap().entrySet().iterator
    var ok = true
    while(it.hasNext && ok) {
      val states = it.next().getValue
      val stateIter = states.iterator
      while(ok && stateIter.hasNext) {
        val next = stateIter.next
        ok = !order.gt(next, state)
        if(!ok) {
//          println(next + " dominates " + state)
        }
      }
    }

    if(ok) {
      /*
      val it = set.tailMap(bin(state), true).values().iterator
      while(it.hasNext) {
        val states = it.next()
        val stateIter = states.iterator
        while(stateIter.hasNext) {
          val next = stateIter.next
          if(order.gt(state, next)) {
            println("Pruning " + next)
            stateIter.remove()
          }
        }
      }
      */
      var mybin = set.get(cost)
      if(mybin eq null ){
        mybin = new util.ArrayList[T]()
        set.put(cost, mybin)
      }
      mybin.add(state)
      size += 1
    }


    ok
  }
}

object NaiveSkyline {
  def oracle[T:PartialOrdering]: Oracle.Factory[T] = new Factory[T] {
    def make: Oracle[T] = new NaiveSkyline[T]
  }
}
