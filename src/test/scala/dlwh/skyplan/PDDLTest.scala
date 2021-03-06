package dlwh.skyplan

import org.scalatest._
import org.scalatest.junit._
import org.junit.runner.RunWith
import io.Source
import dlwh.search.Domain
import com.sun.net.httpserver.Authenticator.Success

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class PDDLTest extends FunSuite {
  def slurpResource(str: String) =  {
    Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(str)).mkString
  }

  test("Read in predicates") {
  PDDL.DomainReader.parse(PDDL.DomainReader.predicates, """  (:predicates
        (can-move ?from-waypoint ?to-waypoint)
        (is-visible ?objective ?waypoint)
        (is-in ?sample ?waypoint)
        (been-at ?rover ?waypoint)
        (carry ?rover ?sample)
        (at ?rover ?waypoint)
        (is-dropping-dock ?waypoint)
        (taken-image ?objective)
        (stored-sample ?sample)
        (objective ?objective)
        (waypoint ?waypoint)
        (sample ?sample)
        (rover ?rover)
        (empty ?rover)
    )""")
  }

  test("Read in raw domain") {
    PDDL.DomainReader.parse(PDDL.DomainReader.domain, "(define (domain rover-domain))").get
  }

  test("Read in raw problem") {
    PDDL.parseProblem(
      """
        |(define (problem rover-single)
        |    (:domain
        |        rover-domain
        |    )
        |
        |    (:objects
        |        waypoint1 waypoint2 waypoint3 waypoint4 waypoint5 waypoint6
        |        waypoint7 waypoint8 waypoint9 waypoint10 waypoint11 waypoint12
        |
        |        sample1 sample2 sample3 sample4 sample5 sample6 sample7 sample8
        |        sample9
        |
        |        objective1 objective2 objective3 objective4
        |
        |        rover1
        |    )
        |
        |    (:init
        |        (= (sample-capacity) 2)

        |        )
        |        )

      """.stripMargin)


  }

  test("read in action") {
    import PDDL.DomainReader._
    PDDL.DomainReader.parse(PDDL.DomainReader.action,
      """ (:action move
      :parameters
          (?rover
           ?from-waypoint
           ?to-waypoint)

      :precondition
          (and
              (rover ?rover)
              (waypoint ?from-waypoint)
              (waypoint ?to-waypoint)
              (at ?rover ?from-waypoint)
              (can-move ?from-waypoint ?to-waypoint)
              (> (battery-amount ?rover) 8))

      :effect
          (and
          (decrease (battery-amount ?rover) 8)
              (at ?rover ?to-waypoint)
              (been-at ?rover ?to-waypoint)
              (not (at ?rover ?from-waypoint))
              )
  )""") match {
      case PDDL.DomainReader.Success(a, _) =>
      case PDDL.DomainReader.Failure(a, _) => throw new RuntimeException(a)
    }
  }

  test("Read in strips rover domain") {
    val input = slurpResource("examples/rover/strips/domain.pddl")
    PDDL.parseDomain(input)
  }

  test("Read in numeric rover domain") {
    val input = slurpResource("examples/rover/numeric/domain.pddl")
    PDDL.parseDomain(input)
  }

  test("Read in time rover domain") {
    val input = slurpResource("examples/rover/time/domain.pddl")
    PDDL.parseDomain(input)
  }


  test("Read in strips rover problem") {
    val input = slurpResource("examples/rover/strips/problem.pddl")
    PDDL.parseProblem(input)
  }

  test("Read in numeric rover problem") {
    val input = slurpResource("examples/rover/numeric/problem.pddl")
    PDDL.parseProblem(input)
  }

  test("Read in time rover problem") {
    val input = slurpResource("examples/rover/time/problem.pddl")
    PDDL.parseProblem(input)
  }

  test("Types") {
    PDDL.DomainReader.parse(PDDL.DomainReader.types,
      """ (:types place vehicle - store
        | resource)
        | """.stripMargin).get
  }


  test("Read in settlers ") {
    val input = slurpResource("examples/pddl/settlers/domain.pddl")
    PDDL.parseDomain(
      """
        |(define (domain civ)
        |  (:requirements :fluents :typing :conditional-effects)
        |  (:types place vehicle - store
        |	  resource)

        |
        |   )
      """.stripMargin)
    PDDL.parseDomain(input)

    for (i <- 1 until 20) {
      val input = slurpResource("examples/pddl/settlers/pfile"+i)
      PDDL.parseProblem(input)
    }
  }

}
