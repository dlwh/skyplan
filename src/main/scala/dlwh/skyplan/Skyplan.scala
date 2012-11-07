package dlwh.skyplan

import dlwh.search.{SearchProblem, SkylineSearch, AStarSearch}
import io.Source
import collection.immutable.IndexedSeq
import java.util

/**
 * 
 * @author dlwh
 */
object Skyplan {
  def findPlan(inst: ProblemInstance) = {
    implicit val ordering = makeOrdering(inst)
     def h(s: State) = 0.0

     def succ(s: State, cost: Double): IndexedSeq[(State, Option[Grounded[IndexedAction]], Double)] = {
       val do_actions = s.relevantActions.map { case grounding =>
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

     new SkylineSearch[State, Option[Grounded[IndexedAction]]].search(SearchProblem(inst.initialState, succ _, {(s: State) => inst.goal.holds(s, s.makeContext())}, heuristic = h _))
   }


  def makeOrdering(inst: ProblemInstance) = {
    val checker = inst.dominanceChecker
    new PartialOrdering[State] {
      def tryCompare(x: State, y: State): Option[Int] = {
        checker.compareStates(x, y) match {
          case NonComparable => None
          case IsDominated => Some(-1)
          case Equals => Some(0)
          case Dominates => Some(1)
        }
      }

      def lteq(x: State, y: State): Boolean = checker.isDominatedBy(x, y)
    }
  }

  def main(args: Array[String]) {
    def slurpResource(str: String) =  {
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(str)).mkString
    }
//    val input = slurpResource("examples/pddl/settlers/domain.pddl")
//    val input2 = slurpResource("examples/pddl/settlers/pfile0")
//    val input = slurpResource("examples/pddl/woodworking/p01-domain.pddl")
//    val input2 = slurpResource("examples/pddl/woodworking/p01.pddl")
    val input = slurpResource("examples/pddl/openstacks/p01-domain.pddl")
    val input2 = slurpResource("examples/pddl/openstacks/p01.pddl")
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
