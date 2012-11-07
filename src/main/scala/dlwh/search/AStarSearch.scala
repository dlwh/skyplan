package dlwh.search

import collection.mutable


/**
 * 
 * @author dlwh
 */
class AStarSearch[T, Action](treeSearch: Boolean = false) {
  def search(problem: SearchProblem[T, Action]):Option[(Path[T, Action], Double)] = {
    import problem._
    case class State(path: Path[T, Action], cost: Double, heur: Double) {
      def t = path.head
      def next: IndexedSeq[(T, Action, Double)] = successors(path.head, cost)

      def estimate = cost + heur
    }

    implicit val ordState: Ordering[State] = Ordering[Double].on(-_.estimate)

    val queue = new mutable.PriorityQueue[State]()
    val visited = new mutable.HashSet[T]()

    queue += State(End(init), 0, heuristic(init))
    while(queue.nonEmpty) {
      val cur: State = queue.dequeue()
      val t = cur.t

      if(isGoal(t)) {
        return Some(cur.path.reverse -> cur.cost)
      }

      if(!treeSearch)
        visited += t

      for( (s,a,c) <- cur.next) {
        if(treeSearch || !visited(s))
          queue += State(cur.path.prepend(s, a), c + cur.cost, heuristic(s))
      }

    }
    None
  }

}


