package dlwh.skyplan

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import io.Source

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class HierarchicalSkyplanTest  extends FunSuite {
  def slurpResource(str: String) =  {
    Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(str)).mkString
  }

  /*
  test("Strips Rover") {
    val input = slurpResource("examples/rover/strips/problem.pddl")
    val input2 = slurpResource("examples/rover/strips/domain.pddl")
    val problem = PDDL.parseProblem(input)
    val domain = PDDL.parseDomain(input2)


    val instance = ProblemInstance.fromPDDL(domain, problem)
    val init = instance.initialState
    val plan = AStarPlanner.findPlan(instance)
    assert(plan.nonEmpty,plan)
  }

  test("Settlers 0") {
    val input = slurpResource("examples/pddl/settlers/domain.pddl")
    val input2 = slurpResource("examples/pddl/settlers/pfile0")
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)


    try {

      val instance = ProblemInstance.fromPDDL(domain, problem).deleteFreeVersion
      val plan = Skyplan.findPlan(instance)
      assert(plan.nonEmpty,plan)
    } catch {
      case e =>
        e.printStackTrace()
      throw e
    }
  }

  */
  test("Woodworking 1") {
    val input = slurpResource("examples/pddl/tempo-sat/woodworking/p01-domain.pddl")
    val input2 = slurpResource("examples/pddl/tempo-sat/woodworking/p01.pddl")
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)


    try {

      val instance = ProblemInstance.fromPDDL(domain, problem)
      val plan = HierarchicalSkyplan.findPlan(instance, true)
      assert(plan.nonEmpty,plan)
//      println(plan.get._1)
      assert(plan.get._2._1 === 50.0)
    } catch {
      case e =>
        e.printStackTrace()
        throw e
    }
  }


  test("Elevators 0 ") {
    val input = slurpResource("examples/pddl/tempo-sat/elevators/p00.pddl")
    val input2 = slurpResource("examples/pddl/tempo-sat/elevators/p00-domain.pddl")
    val problem = PDDL.parseProblem(input)
    val domain = PDDL.parseDomain(input2)

    try {

      val instance = ProblemInstance.fromPDDL(domain, problem)
      val plan = HierarchicalSkyplan.findPlan(instance, true)
      assert(plan.nonEmpty,plan)
      //      println(plan.get._1)
      assert(plan.get._2._1 === 14.0)
    } catch {
      case e =>
        e.printStackTrace()
        throw e
    }
  }

//  test("Elevators 1") {
//    val input = slurpResource("examples/pddl/tempo-sat/elevators/p01.pddl")
//    val input2 = slurpResource("examples/pddl/tempo-sat/elevators/p01-domain.pddl")
//    val problem = PDDL.parseProblem(input)
//    val domain = PDDL.parseDomain(input2)
//
//    try {
//
//      val instance = ProblemInstance.fromPDDL(domain, problem)
//      val plan = HierarchicalSkyplan.findPlan(instance, true)
//      assert(plan.nonEmpty,plan)
//            println(plan.get._1)
//      assert(plan.get._2._1 === 14.0)
//    } catch {
//      case e =>
//        e.printStackTrace()
//        throw e
//    }
//  }
}
