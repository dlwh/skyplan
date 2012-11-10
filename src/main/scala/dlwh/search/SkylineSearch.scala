package dlwh.search

import collection.mutable
import java.util

/**
 * 
 * @author dlwh
 */
class SkylineSearch[T, Action](factory: Oracle.Factory[T], treeSearch: Boolean = false) {
  def search(problem: SearchProblem[T, Action]):Option[(Path[T, Action], Double)] = {
    import problem._
    case class State(path: Path[T, Action], cost: Double, heur: Double) {
      def t = path.head
      def next = successors(path.head, cost)

      def estimate = cost + heur

      override def toString = t.toString
    }

    val skyline = factory.make

    implicit val ordState: Ordering[State] = Ordering[Double].on(-_.estimate)

    val queue = new mutable.PriorityQueue[State]()
    val visited = new mutable.HashSet[T]()
    var numAdded,numTried = 0

    val initialState:State = State(End(init), 0, heuristic(init))
    queue += initialState
    skyline.accepts(init, 0.0)
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
        if( (treeSearch || !visited(s)) && skyline.accepts(s, c + cur.cost)) {
          numAdded += 1
          queue += nextState
        }
      }

    }
    None
  }
}



