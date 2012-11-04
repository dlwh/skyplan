package dlwh.search

import collection.mutable
import util.control.Breaks._


/**
 * 
 * @author dlwh
 */
class FastForwardSearch[T] {
  def search[Action](init: T,
                successors: (T,Double)=>IndexedSeq[(T, Action, Double)],
                isGoal: T=>Boolean,
                h: T=>Double = ( (x:T) => 0.0),
                treeSearch: Boolean = false):Option[(Path[T, Action], Double)] = {
    case class State(path: Path[T, Action], cost: Double, heur: Double) {
      def t = path.head
      def next = successors(path.head, cost)

      def estimate = heur
    }

    val visited = mutable.HashSet[T]()

    var current: State = new State(End(init), 0, h(init))
    while(current != null && !isGoal(current.t)) {
      breakable {
        val queue = mutable.Queue[State]()
        queue += current
        // bfs until we find something that improves the heuristic
        while(!queue.isEmpty) {
          val next = queue.dequeue()
          if(isGoal(next.t)) return Some(next.path.reverse -> next.cost)

          for((n,a,cost) <- next.next if !cost.isInfinite && !visited(n)) {
            val hh = h(n)
            val state = State(next.path.prepend(n, a), cost + next.cost, hh)
            if(isGoal(n)) {
              return Some(state.path.reverse -> state.cost)
            } else if(hh < current.estimate) {
              current = state
              break()
            } else if(!hh.isInfinite) {
              queue += state
            }
            visited += n
          }

        }
        // if we make it here, search fails
        current = null
      }
    }
    None
  }

}


