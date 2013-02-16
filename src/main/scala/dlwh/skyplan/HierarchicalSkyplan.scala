package dlwh.skyplan

import dlwh.search._
import io.Source

/**
 * 
 * @author dlwh
 */
object HierarchicalSkyplan {
  def findPlan(inst: ProblemInstance, skyplan: Boolean = true): Option[(Path[State, Option[Grounded[IndexedAction]]], (Double, Int))] = {
    implicit val ordering = Skyplan.makeOrdering(inst)
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

    val projected = projectionHierarchy(inst).map{ inst2 =>
      SearchProblem(inst.initialState, {succ(_:State, _: Double, inst2.goal)}, {(s: State) => inst2.goal.holds(s, s.makeContext())})
    }

    val oracle = if(skyplan) StateSkyline.factory(inst) else Oracle.allAllowed[State]
    val search = new HierarchicalSkylineSearch[State, Option[Grounded[IndexedAction]]](oracle)
//        val search = new HierarchicalSkylineSearch[State, Option[Grounded[IndexedAction]]](Oracle.allAllowed, treeSearch = true)
    search.search(projected, IndexedSeq.fill(projected.length - 1)(identity))
  }

  def projectionHierarchy(inst: ProblemInstance):IndexedSeq[ProblemInstance] = inst.goal match {
    case AndCondition(cs) if cs.length > 4 =>  inst +: projectionHierarchy(inst.copy(goal=AndCondition(cs.take(4))))
    case _ =>  IndexedSeq(inst)
  }



  def warmupHotspot() {
    def slurpResource(str: String) =  {
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(str)).mkString
    }
    val domainFile = "examples/pddl/tempo-sat/woodworking/p03-domain.pddl"
    val problemFile = "examples/pddl/tempo-sat/woodworking/p03.pddl"
    val input = slurpResource(domainFile)
    val input2 = slurpResource(problemFile)
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)

    val maxRuns = 3
    val instance = ProblemInstance.fromPDDL(domain, problem)
    for (i <- 0 until maxRuns) {
      val start = System.currentTimeMillis()
      val plan = HierarchicalSkyplan.findPlan(instance)
      val stop = System.currentTimeMillis()
      assert(plan.nonEmpty,plan)
      println("Hotspot Warmup Run " + (i+1))
      println("Total time: " + (stop - start))
      println()
    }
  }


  def main(args: Array[String]) {
    def slurpResource(str: String) =  {
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(str)).mkString
    }
    val skyplan = args.length < 1 || args(0).toBoolean
    val domainFile = if (args.length >= 3) args(1) else "examples/pddl/tempo-sat/woodworking/p03-domain.pddl"
    val problemFile = if (args.length >= 3) args(2) else "examples/pddl/tempo-sat/woodworking/p03.pddl"
    val input = slurpResource(domainFile)
    val input2 = slurpResource(problemFile)
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)

    if(skyplan) {
      println("Skyplan! " + problemFile)
    } else {
      println("A* " + problemFile)
    }

    def average(seq: IndexedSeq[Double]) = {
      if (seq.size > 0) {
        if (seq.size <= 2) seq.sum / seq.size
        else {
          val newSeq = seq.sorted.drop(1).dropRight(1)
          newSeq.sum / newSeq.size
        }
      } else 0.0
    }

    try {
      warmupHotspot()
      val maxRuns = if (args.length >= 4) args(3).toInt else 20
      val instance = ProblemInstance.fromPDDL(domain, problem)
      var times = IndexedSeq.empty[Double]
      var nodes = IndexedSeq.empty[Double]
      var costs = IndexedSeq.empty[Double]
      val init = instance.initialState
      for (i <- 0 until maxRuns) {
        val start = System.currentTimeMillis()
        val plan = HierarchicalSkyplan.findPlan(instance, skyplan=skyplan)
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

