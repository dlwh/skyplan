package dlwh.search

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class HierarchicalAStarSearchTest extends FunSuite {
  test("wikipedia, no projection") {
    val costs = Map(
      's -> IndexedSeq('d -> 2.0, 'a -> 1.5),
      'a -> IndexedSeq('b -> 2.0),
      'b -> IndexedSeq('c -> 3.0),
      'c -> IndexedSeq('g -> 4.0),
      'd -> IndexedSeq('e -> 3.0),
      'e -> IndexedSeq('g -> 2.0)
    )

    def succ(a: Symbol, x: Double) = {
      costs(a).map{ case (s, c) => (s, (), c)}
    }

    val h = Map(
      's -> 5.5,
      'a -> 4.0,
      'b -> 2.0,
      'c -> 4.0,
      'd -> 4.5,
      'e -> 2.0,
      'g -> 0.0
    )


    val astar = new HierarchicalAStarSearch[Symbol, Unit]()
    val problem = SearchProblem('s, succ _, { (_: Symbol) == 'g }, h)
    val (path, cost) = astar.search(IndexedSeq(problem), IndexedSeq()).get
    val syms = path.toStateList
    assert(syms === List('s, 'd, 'e, 'g))
    assert(cost === 7.0)
  }

  test("wikipedia, identity projection") {
    val costs = Map(
      's -> IndexedSeq('d -> 2.0, 'a -> 1.5),
      'a -> IndexedSeq('b -> 2.0),
      'b -> IndexedSeq('c -> 3.0),
      'c -> IndexedSeq('g -> 4.0),
      'd -> IndexedSeq('e -> 3.0),
      'e -> IndexedSeq('g -> 2.0),
      'g -> IndexedSeq()
    )

    def succ(a: Symbol, x: Double) = {
      costs(a).map{ case (s, c) => (s, (), c)}
    }

    val h = Map(
      's -> 5.5,
      'a -> 4.0,
      'b -> 2.0,
      'c -> 4.0,
      'd -> 4.5,
      'e -> 2.0,
      'g -> 0.0
    )


    val astar = new HierarchicalAStarSearch[Symbol, Unit]()
    val problem = SearchProblem('s, succ _, { (_: Symbol) == 'g }, h)
    val (path, cost) = astar.search(IndexedSeq(problem, problem), IndexedSeq(identity[Symbol])).get
    val syms = path.toStateList
    assert(syms === List('s, 'd, 'e, 'g))
    assert(cost === 7.0)
  }

  test("wikipedia, slightly trivial projection") {
    val costs = Map(
      's -> IndexedSeq('d -> 2.0, 'a -> 1.5),
      'a -> IndexedSeq('b -> 2.0),
      'b -> IndexedSeq('c -> 3.0),
      'c -> IndexedSeq('g -> 4.0),
      'd -> IndexedSeq('e -> 3.0),
      'e -> IndexedSeq('g -> 2.0),
      'g -> IndexedSeq(),
      // projected
      'πs -> IndexedSeq('πe -> 3.0, 'πb -> 2.0),
      'πe -> IndexedSeq('πg -> 2.0),
      'πb -> IndexedSeq('πg -> 3.0),
      'πg -> IndexedSeq()
    )

    val proj = Map(
      's -> 'πs,
      'a -> 'πb,
      'b -> 'πb,
      'c -> 'πb,
      'g -> 'πg,
      'd -> 'πe,
      'e -> 'πe
    )


    def succ(a: Symbol, x: Double) = {
      costs(a).map{ case (s, c) => (s, (), c)}
    }

    val astar = new HierarchicalAStarSearch[Symbol, Unit]()
    val problem = SearchProblem('s, succ _, { (_: Symbol) == 'g })
    val πroblem = SearchProblem('πs, succ _, { (_: Symbol) == 'πg })
    val (path, cost) = astar.search(IndexedSeq(problem, πroblem), IndexedSeq(proj)).get
    val syms = path.toStateList
    assert(syms === List('s, 'd, 'e, 'g))
    assert(cost === 7.0)
  }



  test("wikipedia, slightly trivial projection fails if costs are not projected right.") {
    val costs = Map(
      's -> IndexedSeq('d -> 2.0, 'a -> 1.5),
      'a -> IndexedSeq('b -> 2.0),
      'b -> IndexedSeq('c -> 3.0),
      'c -> IndexedSeq('g -> 4.0),
      'd -> IndexedSeq('e -> 3.0),
      'e -> IndexedSeq('g -> 2.0),
      'g -> IndexedSeq(),
      // projected
      // n.b. πs -> πe is wrong.
      'πs -> IndexedSeq('πe -> 100.0, 'πb -> 2.0),
      'πe -> IndexedSeq('πg -> 2.0),
      'πb -> IndexedSeq('πg -> 3.0),
      'πg -> IndexedSeq()
    )

    val proj = Map(
      's -> 'πs,
      'a -> 'πb,
      'b -> 'πb,
      'c -> 'πb,
      'g -> 'πg,
      'd -> 'πe,
      'e -> 'πe
    )


    def succ(a: Symbol, x: Double) = {
      costs(a).map{ case (s, c) => (s, (), c)}
    }

    val astar = new HierarchicalAStarSearch[Symbol, Unit]()
    val problem = SearchProblem('s, succ _, { (_: Symbol) == 'g })
    val πroblem = SearchProblem('πs, succ _, { (_: Symbol) == 'πg })
    val (path, cost) = astar.search(IndexedSeq(problem, πroblem), IndexedSeq(proj)).get
    val syms = path.toStateList
    assert(syms != List('s, 'd, 'e, 'g), syms + " was optimal even though the projection was really wrong?")
  }

}
