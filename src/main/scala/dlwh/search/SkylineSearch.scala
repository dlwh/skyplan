package dlwh.search

import collection.mutable
import java.util

/**
 * 
 * @author dlwh
 */
class SkylineSearch[T:PartialOrdering, Action](treeSearch: Boolean = false) {
  def search(problem: SearchProblem[T, Action]):Option[(Path[T, Action], Double)] = {
    import problem._
    case class State(path: Path[T, Action], cost: Double, heur: Double) {
      def t = path.head
      def next = successors(path.head, cost)

      def estimate = cost + heur

      override def toString = t.toString
    }

    val o = implicitly[PartialOrdering[T]]

    implicit val po = new PartialOrdering[State] {
      def tryCompare(x: State, y: State): Option[Int] = {
        o.tryCompare(x.t,y.t)
      }

      def lteq(x: State, y: State): Boolean = o.lteq(x.t,y.t)
    }
    val skyline = new Skyline[State](_.estimate.toInt)(po)

    implicit val ordState: Ordering[State] = Ordering[Double].on(-_.estimate)

    val queue = new mutable.PriorityQueue[State]()
    val visited = new mutable.HashSet[T]()
    var numAdded,numTried = 0

    val initialState = State(End(init), 0, heuristic(init))
    queue += initialState
    skyline tryAdd initialState
    var numPopped = 0
    while(queue.nonEmpty) {
      val cur = queue.dequeue()
      val t = cur.t
      numPopped += 1
      if(numPopped % 100 == 0) {
        println(cur.estimate)
        println(numAdded + "/" + numTried + " " + skyline.size)
      }

      if(isGoal(t)) {
        return Some(cur.path.reverse -> cur.cost)
      }

      if(!treeSearch)
        visited += t

      for( (s,a,c) <- cur.next) {
        val nextState = State(cur.path.prepend(s, a), c + cur.cost, heuristic(s))
        numTried += 1
        if( (treeSearch || !visited(s)) && skyline.tryAdd(nextState)) {
          numAdded += 1
          queue += nextState
        }
      }

    }
    None
  }
}


class Skyline[T:PartialOrdering](bin: T=>Int) {
  private val order = implicitly[PartialOrdering[T]]
  def size = set.size
  private val set = new java.util.TreeMap[Int, util.ArrayList[T]]()

  def tryAdd(state: T) = {
    val headSet = set.headMap(bin(state), true)
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
      var mybin = set.get(bin(state))
      if(mybin eq null ){
        mybin = new util.ArrayList[T]()
        set.put(bin(state), mybin)
      }
      mybin.add(state)
    }


    ok
  }
}
