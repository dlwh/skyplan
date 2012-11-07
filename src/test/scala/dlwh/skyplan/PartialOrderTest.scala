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
class PartialOrderTest  extends FunSuite {
  def slurpResource(str: String) =  {
    Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(str)).mkString
  }

  test("New axiom") {
    val input = slurpResource("examples/pddl/settlers/domain.pddl")
    val input2 = slurpResource("examples/pddl/settlers/pfile1")
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)


    try {

      val instance = ProblemInstance.fromPDDL(domain, problem)
      val init: State = instance.initialState
      val compare = init.copy
      val axiom = instance.predicates.ground("has-cabin", IndexedSeq(instance.objects.index("location2")))
      compare.axioms add axiom

      val checker = new DominanceChecker(instance)
      val cmp = checker.compareStates(init, compare)

      assert(cmp === IsDominated)
      assert((init isEqualToOrDominatedBy compare) == true)
      assert((compare isEqualToOrDominatedBy init) == false)

    } catch {
      case e =>
        e.printStackTrace()
        throw e
    }
  }

  test("Good resource") {
    val input = slurpResource("examples/pddl/settlers/domain.pddl")
    val input2 = slurpResource("examples/pddl/settlers/pfile1")
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)


    try {

      val instance = ProblemInstance.fromPDDL(domain, problem)
      val init: State = instance.initialState
      val compare = init.copy
      val resource = instance.valFuns.ground("housing", IndexedSeq(instance.objects.index("location0")))
      compare.resources(resource) += 2

      val checker = new DominanceChecker(instance)
      val cmp = checker.compareStates(init, compare)

      assert(cmp === IsDominated)
      assert((init isEqualToOrDominatedBy compare) == true)
      assert((compare isEqualToOrDominatedBy init) == false)

      compare.resources(instance.valFuns.ground("labour", IndexedSeq())) += 1

      val cmp2 = checker.compareStates(init, compare)
      assert(cmp2 === NonComparable)
      assert((init isEqualToOrDominatedBy compare) == false)
      assert((compare isEqualToOrDominatedBy init) == false)

    } catch {
      case e =>
        e.printStackTrace()
        throw e
    }

  }

  test("Bad resource") {
    val input = slurpResource("examples/pddl/settlers/domain.pddl")
    val input2 = slurpResource("examples/pddl/settlers/pfile1")
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)


    try {

      val instance = ProblemInstance.fromPDDL(domain, problem)
      val init: State = instance.initialState
      val compare = init.copy
      val resource = instance.valFuns.ground("labour", IndexedSeq.empty[Int])
      compare.resources(resource) += 2

      val checker = new DominanceChecker(instance)
      val cmp = checker.compareStates(init, compare)

      assert(cmp === Dominates)
      assert((init isEqualToOrDominatedBy compare) == false)
      assert((compare isEqualToOrDominatedBy init) == true)

    } catch {
      case e =>
        e.printStackTrace()
        throw e
    }

  }

  test("Irrelevant resource") {
    val input = slurpResource("examples/pddl/settlers/domain.pddl")
    val input2 = slurpResource("examples/pddl/settlers/pfile1")
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)


    try {

      val instance = ProblemInstance.fromPDDL(domain, problem)
      val init: State = instance.initialState
      val compare = init.copy
      val resource = instance.valFuns.ground("pollution", IndexedSeq.empty[Int])
      compare.resources(resource) += 2

      val checker = new DominanceChecker(instance)
      val cmp = checker.compareStates(init, compare)

      assert(cmp === Equals)
      assert((init isEqualToOrDominatedBy compare) == true)
      assert((compare isEqualToOrDominatedBy init) == true)

    } catch {
      case e =>
        e.printStackTrace()
      throw e
    }
  }

  test("Action queue") {
    val input = slurpResource("examples/pddl/settlers/domain.pddl")
    val input2 = slurpResource("examples/pddl/settlers/pfile1")
    val domain = PDDL.parseDomain(input)
    val problem = PDDL.parseProblem(input2)


    try {

      val instance = ProblemInstance.fromPDDL(domain, problem)
      val init: State = instance.initialState
      val compare = init.copy
      val (a, args) = init.possibleActions.iterator.next()
      val action = instance.actions.ground(a, args)
      compare.pendingActions.enqueue(action, 10)

      val checker = new DominanceChecker(instance)
      var cmp = checker.compareStates(init, compare)
      assert(cmp === IsDominated)

      init.pendingActions.enqueue(action, 6)
      cmp = checker.compareStates(init, compare)
      assert(cmp === Dominates)

      compare.pendingActions.enqueue(action, 2)
      cmp = checker.compareStates(init, compare)
      assert(cmp === IsDominated)

      init.pendingActions.enqueue(action, 8)
      cmp = checker.compareStates(init, compare)
      assert(cmp === NonComparable)

      compare.pendingActions.enqueue(action, 12)
      cmp = checker.compareStates(init, compare)
      assert(cmp === NonComparable)

      compare.pendingActions.enqueue(action, 8)
      cmp = checker.compareStates(init, compare)
      assert(cmp === IsDominated)

      init.pendingActions.enqueue(action, 10)
      compare.pendingActions.enqueue(action, 6)
      init.pendingActions.enqueue(action, 2)
      init.pendingActions.enqueue(action, 12)
      cmp = checker.compareStates(init, compare)
      assert(cmp === Equals)

      init.pendingActions.enqueue(action, 5000)
      cmp = checker.compareStates(init, compare)
      assert(cmp === Dominates)

    } catch {
      case e =>
        e.printStackTrace()
        throw e
    }
  }
}

