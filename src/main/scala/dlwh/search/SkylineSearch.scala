package dlwh.search

import collection.mutable
import dlwh.skyplan.{PriorityQueueFringe, DumbSkylineFringe}


/**
 * 
 * @author dlwh
 */
class SkylineSearch[T:PartialOrdering] {
  def search[Action](init: T,
                successors: (T,Double)=>IndexedSeq[(T, Action, Double)],
                isGoal: T=>Boolean,
                h: T=>Double = ( (x:T) => 0.0),
                treeSearch: Boolean = false):Option[(Path[T, Action], Double)] = {
    case class State(path: Path[T, Action], cost: Double, heur: Double) {
      def t = path.head
      def next = successors(path.head, cost)

      def estimate = cost + heur
    }

    implicit val ordState: Ordering[State] = Ordering[Double].on(-_.estimate)
    val origOrder = implicitly[PartialOrdering[T]]
    implicit val partialOrdState: PartialOrdering[State] = new PartialOrdering[State] {
      def tryCompare(x: State, y: State): Option[Int] = origOrder.tryCompare(x.t, y.t)

      def lteq(x: State, y: State): Boolean = origOrder.lteq(x.t, y.t)
    }

    val queue = new DumbSkylineFringe[State](new PriorityQueueFringe[State])
    val visited = new mutable.HashSet[T]()

    val state = State(End(init), 0, h(init))
    queue.enqueue(state, state.estimate)
    while(!queue.isEmpty) {
      val cur = queue.dequeue().get._1
      val t = cur.t
      println(cur.estimate)

      if(isGoal(t)) {
        return Some(cur.path.reverse -> cur.cost)
      }

      if(!treeSearch)
        visited += t

      for( (s,a,c) <- cur.next) {
        if(treeSearch || !visited(s)) {
            val state = State(cur.path.prepend(s, a), c + cur.cost, h(s))
            queue.enqueue(state, state.estimate)
          }
      }

    }
    None
  }

}


