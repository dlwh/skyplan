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

    val dependencies = new mutable.HashMap[T, ArrayBuffer[State]]() {
      override def apply(key: T) = getOrElseUpdate(key, new ArrayBuffer[State]())
    }
    val heuristics: Array[mutable.Map[T, Double]] = Array.fill(numLevels)(mutable.Map[T, Double]())

    val queue = new mutable.PriorityQueue[State]()

    def enqueue(state: State) {
      import state._
      if(level == proj.length) {
        queue += state.copy(heur=instances.last.heuristic(state.t))
      } else {
        val projected = proj(level)(state.t)
        heuristics(level+1).get(projected) match {
          case Some(h) => queue += state.copy(heur=h)
          case None => dependencies(projected) += state
        }
      }
    }

    val visited = Array.fill(numLevels)(mutable.HashSet[T]())

    var init: T = instances(0).init
    for(i <- 0 until proj.length) {
      val newInit = proj(i)(init)
      dependencies(newInit) += State(i, init, 0, None, Double.NaN)
      init = newInit
    }
    queue += State(proj.length, init, 0, None, 0)
    while(queue.nonEmpty) {
      val cur: State = queue.dequeue()
      val t = cur.t

      if(instances(cur.level).isGoal(t)) {
        if(cur.level == 0)
          return Some(cur.toPath.reverse -> cur.cost)
        val cost = cur.cost
        for(ss <- cur.states) {
          heuristics(ss.level).getOrElseUpdate(ss.t, cost - ss.cost)
          for(deps <- dependencies.get(ss.t))  {
            for(d <- deps) {
              queue += d.copy(heur=cost - ss.cost)
            }
            dependencies -= ss.t
          }
        }
      }
      if(!treeSearch)
        visited(cur.level) += t

      for( (s,a,c) <- cur.next) {
        if(treeSearch || !visited(cur.level)(s))
          enqueue(State(cur.level, s, c + cur.cost, Some(a, cur)))
      }

    }
    None
  }

}
