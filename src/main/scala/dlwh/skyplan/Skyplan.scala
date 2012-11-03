package dlwh.skyplan

import dlwh.search.{SkylineSearch, AStarSearch}
import io.Source

/**
 * 
 * @author dlwh
 */
object Skyplan {
  def findPlan(inst: ProblemInstance) = {
    val checker = inst.dominanceChecker
    implicit val ordering = new PartialOrdering[State] {
      def tryCompare(x: State, y: State): Option[Int] = {
        checker.compareStates(x,y) match {
          case NonComparable => None
          case IsDominated => Some(-1)
          case Equals => Some(0)
          case Dominates => Some(1)
        }
      }

      def lteq(x: State, y: State): Boolean = checker.isDominatedBy(x,y)
    }
     def h(s: State) = 0.0

     def succ(s: State, cost: Double) = {
       s.relevantActions.map { case grounding =>
         val a = grounding.t
         val list = grounding.args
         val c = s.copy
         val grounded = s.problem.actions.ground(a, list)
//         println(s, grounding)
         c.applyAction(grounded, a.durationOf(s, list))
         (c, a, c.cost - cost)
       }.toIndexedSeq
     }
     new SkylineSearch[State].search(inst.initialState, succ _, {(s: State) => inst.goal.holds(s, s.makeContext())}, h = h _)
   }


  def main(args: Array[String]) {
    def slurpResource(str: String) =  {
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(str)).mkString
    }
    val input = slurpResource("examples/pddl/settlers/domain.pddl")
    val input2 = slurpResource("examples/pddl/settlers/pfile0")
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)


    try {

      val instance = ProblemInstance.fromPDDL(domain, problem)
      val init = instance.initialState
      val plan = Skyplan.findPlan(instance)
      assert(plan.nonEmpty,plan)
      println(plan)
    } catch {
      case e =>
        e.printStackTrace()
        throw e
    }
  }
}
