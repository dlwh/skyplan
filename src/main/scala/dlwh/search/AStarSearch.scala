package dlwh.search

import collection.mutable


/**
 * 
 * @author dlwh
 */
object AStarSearch {
  def search[T, Action](init: T,
                successors: T=>IndexedSeq[(T, Action, Double)],
                isGoal: T=>Boolean,
                h: T=>Double = ( (x:T) => 0.0),
                treeSearch: Boolean = false):Option[(Path[T, Action], Double)] = {
    case class State(path: Path[T, Action], cost: Double, heur: Double) {
      def t = path.head
      def next = successors(path.head)

      def estimate = cost + heur
    }

    implicit val ordState: Ordering[State] = Ordering[Double].on(-_.estimate)

    val queue = new mutable.PriorityQueue[State]()
    val visited = new mutable.HashSet[T]()

    queue += State(End(init), 0, h(init))
    while(queue.nonEmpty) {
      val cur = queue.dequeue()
      val t = cur.t

      if(isGoal(t)) {
        return Some(cur.path.reverse -> cur.cost)
      }

      if(!treeSearch)
        visited += t

      for( (s,a,c) <- successors(t)) {
        if(treeSearch || !visited(s))
          queue += State(cur.path.prepend(s, a), c + cur.cost, h(s))
      }

    }
    None
  }

}
