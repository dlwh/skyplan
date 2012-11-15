package dlwh.skyplan

import dlwh.search.{HierarchicalAStarSearch, SearchProblem, AStarSearch}
import collection.immutable.BitSet
import dlwh.skyplan.Expression.Global
import io.Source
import collection.mutable

/**
 * 
 * @author dlwh
 */
object HierarchicalAStarPlanner {
  def findPlan(inst: ProblemInstance) = {
    def succ(s: State, cost: Double, goal: IndexedCondition): IndexedSeq[(State, Option[Grounded[IndexedAction]], Double)] = {
      val do_actions = s.relevantActions(goal).map { case grounding =>
        val a = grounding.t
        val list = grounding.args
        val c = s.copy
        val grounded = s.problem.actions.ground(a, list)
        c.applyAction(grounded, a.durationOf(s, list))
        (c, Some(grounding), c.cost - cost)
      }.toIndexedSeq

      if(s.hasAction()) do_actions :+ { val s2 = s.copy; s2.elapseTime(); (s2, None, s2.cost - cost)}
      else do_actions
    }

    val projected = projectionHierarchy(inst).map{ inst =>
      SearchProblem(inst.initialState, {succ(_:State, _: Double, inst.goal)}, {(s: State) => inst.goal.holds(s, s.makeContext())})
    }

    val search = new HierarchicalAStarSearch[State, Option[Grounded[IndexedAction]]]
    search.search(projected, IndexedSeq.fill(projected.length - 1)(identity))
  }

    def projectionHierarchy(inst: ProblemInstance):IndexedSeq[ProblemInstance] = inst.goal match {
      case AndCondition(conds) if conds.length > 1 => IndexedSeq(inst, inst.copy(goal=conds.head))
      case _ =>  IndexedSeq(inst)
    }


  def average(seq: IndexedSeq[Double]) = if (seq.size > 0) seq.sum / seq.size else 0.0

  def main(args: Array[String]) {
    def slurpResource(str: String) =  {
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(str)).mkString
    }
    val domainFile = if (args.length >= 2) args(0) else "examples/pddl/woodworking/p03-domain.pddl"
    val problemFile = if (args.length >= 2) args(1) else "examples/pddl/woodworking/p03.pddl"
    val input = slurpResource(domainFile)
    val input2 = slurpResource(problemFile)
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)


    try {
      val maxRuns = 100
      val instance = ProblemInstance.fromPDDL(domain, problem)
      var times = IndexedSeq.empty[Double]
      var nodes = IndexedSeq.empty[Double]
      var costs = IndexedSeq.empty[Double]
      val init = instance.initialState
      for (i <- 0 until maxRuns) {
        val start = System.currentTimeMillis()
        val plan = HierarchicalAStarPlanner.findPlan(instance)
        val stop = System.currentTimeMillis()
        assert(plan.nonEmpty,plan)
        println("Run " + (i+1))
        println("Total time: " + (stop - start))
        println("Nodes popped: " + plan.get._2._2)
        println("Plan length: " + plan.get._2._1)
        println()

        times = times :+ (stop - start).toDouble
        nodes = nodes :+ plan.get._2._2.toDouble
        costs = costs :+ plan.get._2._1
        println("Accumulated averages")
        println("Total time: " + average(times))
        println("Nodes popped: " + average(nodes))
        println("Plan length: " + average(costs))
        println()
      }
    } catch {
      case e =>
        e.printStackTrace()
        throw e
    }
  }
}
