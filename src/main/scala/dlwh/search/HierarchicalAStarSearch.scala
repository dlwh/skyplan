package dlwh.search

import collection.mutable
import collection.mutable.ArrayBuffer

/**
 * 
 * @author dlwh
 */
class HierarchicalAStarSearch[T, Action](treeSearch: Boolean = false) {
  def search(instances: IndexedSeq[SearchProblem[T, Action]], proj: IndexedSeq[T=>T]):Option[(Path[T, Action], Double)] = {
    val numLevels = instances.length
    require(numLevels > 0)
    require(numLevels == proj.length + 1)

    case class State(level: Int, t: T, cost: Double, prev: Option[(Action, State)] = None, heur: Double=  Double.NaN) {
      def next: IndexedSeq[(T, Action, Double)] = instances(level).successors(t, cost)
      def estimate = cost + heur

      def toPath: Path[T, Action] = prev match {
        case None => End(t)
        case Some((a, s)) => Link(t, a, s.toPath)
      }


      lazy val states: List[State] = prev match {
        case Some((_, s)) => this :: s.states
        case None => this :: Nil
      }
    }

    implicit val ordState: Ordering[State] = Ordering[Double].on(-_.estimate)

    val heuristics: Array[mutable.Map[T, Double]] = Array.fill(numLevels)(mutable.Map[T, Double]())


    var popped = 0
    object Searcher {
      def recSearch(level: Int, init: T):Option[(State,Double)] = {
        val queue = new mutable.PriorityQueue[State]()
        val visited = mutable.HashSet[T]()

        enqueue(queue, State(level, init, 0, None, 0))
        while(queue.nonEmpty) {
          val cur = queue.dequeue()
          import cur.{level=>_,_}
          popped += 1
          if(popped % 1000 == 0) {
            println("Popped " + popped)
          }

          if(instances(cur.level).isGoal(t)) {
            return Some(cur -> cost)
          } else if(level != 0 && heuristics(level).contains(t)) {
            return Some(cur -> (cost + heuristics(level)(t)))
          } else {
            if(!treeSearch)
              visited += t

            for( (s,a,c) <- cur.next) {
              if(treeSearch || !visited(s))
                enqueue(queue, State(level, s, cost + c, Some(a, cur)))
            }
          }

        }

        None
      }


      // enqueues elements into the current queue, if h isn't available, it recursively searches
      // to fill it in.
      def enqueue(queue: mutable.PriorityQueue[State], state: State) {
        import state._
        if(level == proj.length) {
          queue += state.copy(heur=instances.last.heuristic(state.t))
        } else {
          val projected = proj(level)(state.t)
          heuristics(level+1).get(projected) match {
            case Some(h) =>
              queue += state.copy(heur=h)
            case None =>
              for( (goal,cost) <- recSearch(level+1, projected)) {
                fillOutHValues(goal, cost)
                if(cost != Double.PositiveInfinity)
                  enqueue(queue, state)
              }
          }
        }
      }

      def fillOutHValues(cur: State, cost: Double) {
        for(ss <- cur.states) {
          heuristics(ss.level).getOrElseUpdate(ss.t, cost - ss.cost)
        }
      }
    }



    for( (goal,cost) <- Searcher.recSearch(0, instances.head.init)) yield goal.toPath.reverse -> cost
  }


}
