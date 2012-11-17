package dlwh.search

import collection.mutable
import collection.mutable.ArrayBuffer

/**
 * 
 * @author dlwh
 */
class HierarchicalSkylineSearch[T, Action](oracleFactory: Oracle.Factory[T], treeSearch: Boolean = false, verbose: Boolean = false) {
  def search(instances: IndexedSeq[SearchProblem[T, Action]], proj: IndexedSeq[T=>T]):Option[(Path[T, Action], (Double, Int))] = {
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
    val skyline = oracleFactory.make

    def fillOutHValues(level: Int, cur: State, cost: Double) {
      for(ss <- cur.states) {
        val c = cost - ss.cost
        if(heuristics(level).contains(ss.t)) {
          return
        }
        heuristics(level)(cur.t) = c
      }
    }

    implicit val ordProblem:Ordering[RecursiveProblem] = Ordering[Double].on(-_.currentEstimate)

    case class HappyException(goal: State, cost: Double) extends RuntimeException

    var popped = 0
    case class RecursiveProblem(level: Int,
                                init: State, initialCost: Double,
                                queue: mutable.PriorityQueue[State] = new mutable.PriorityQueue[State](),
                                recQueue: mutable.PriorityQueue[RecursiveProblem] = new mutable.PriorityQueue[RecursiveProblem](),
                                visited : mutable.HashSet[T] = new mutable.HashSet[T] ) {
      var currentEstimate: Double = 0.0

      def advance():Option[Double] = {
        val selfCost = queue.headOption.map(_.estimate).getOrElse(Double.PositiveInfinity)
        if(recQueue.nonEmpty && selfCost > recQueue.head.currentEstimate) {
          val x = recQueue.dequeue()
          x.advance() match {
            case None =>
              recQueue += x
            case Some(hCost) =>
              if(!hCost.isInfinite) {
                queue += x.init.copy(heur=hCost)
              }
          }
          currentEstimate = queue.headOption.map(_.estimate).getOrElse(Double.PositiveInfinity) min recQueue.headOption.map(_.currentEstimate).getOrElse(Double.PositiveInfinity)
          None
        } else if(!selfCost.isInfinite) {
          dequeue match {
            case None =>
              currentEstimate = Double.PositiveInfinity
              Some(Double.PositiveInfinity)
            case Some(cur) =>
//              println("Pop " + level + " " + cur.cost + " " + cur)
              import cur.{level=>_, _}
              popped += 1

              if(instances(level).isGoal(t)) {
                if(level == 0)
                  throw new HappyException(cur, cost)
                else {
                  fillOutHValues(level, cur, cost - initialCost)
                }
                Some(cost - initialCost)
              } /*else if(heuristics(level).contains(t)) {
                println("close enough!" + cost + " " + heuristics(level)(t) + " " + level)
                fillOutHValues(level, cur, cost + heuristics(level)(t))
                Some(cost + heuristics(level)(t))
              } */ else {
                if(!treeSearch)
                  visited += t

                for( (s,a,c) <- cur.next) {
                  if(treeSearch || !visited(s)) {
                   enqueue(State(level, s, cost + c, Some(a -> cur), Double.NaN))
                  }
                }

                currentEstimate = queue.headOption.map(_.estimate).getOrElse(Double.PositiveInfinity) min recQueue.headOption.map(_.currentEstimate).getOrElse(Double.PositiveInfinity)
                None
              }
          }
        } else {
          currentEstimate = Double.PositiveInfinity
          Some(Double.PositiveInfinity)
        }


      }

      def dequeue: Option[State] = {
        var cur = queue.dequeue()
        while (!skyline.accepts(cur.t, cur.cost)) {
          if (queue.isEmpty) return None
          cur = queue.dequeue()
        }
        val c2 = cur
        Some(c2)
      }

      def enqueue(t: T, cost: Double) {
        enqueue(State(level, t, cost))
        currentEstimate = queue.headOption.map(_.estimate).getOrElse(Double.PositiveInfinity) min recQueue.headOption.map(_.currentEstimate).getOrElse(Double.PositiveInfinity)
      }

      // returns the problem needed
      def enqueue(state: State) {
        import state.{level => _, _}
        if(level == proj.length) {
          queue += state.copy(heur=instances.last.heuristic(t))
        } else {
          val projected = proj(level)(t)
          heuristics(level+1).get(projected) match {
            case Some(h) =>
              val newState = state.copy(heur=h)
              queue += newState
            case None =>
              val newProblem = RecursiveProblem(level+1, state, state.cost)
              newProblem.enqueue(projected, state.cost)
              recQueue += newProblem
          }
        }
      }
    }


    val level0Problem = RecursiveProblem(0, null, 0.0)
    level0Problem.enqueue(instances.head.init, 0.0)
    try {
      while(level0Problem.currentEstimate != Double.PositiveInfinity) {
        level0Problem.advance()
      }
      None
    } catch {
      case HappyException(goal, cost) => Some(goal.toPath.reverse ->(cost, popped))
    }
  }


}
