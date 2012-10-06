package dlwh.search

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class AStarSearchTest extends FunSuite {
  test("Wikipedia test") {
    val costs = Map(
      's -> IndexedSeq('d -> 2.0, 'a -> 1.5),
      'a -> IndexedSeq('b -> 2.0),
      'b -> IndexedSeq('c -> 3.0),
      'c -> IndexedSeq('g -> 4.0),
      'd -> IndexedSeq('e -> 3.0),
      'e -> IndexedSeq('g -> 2.0)
    )

    def succ(a: Symbol) = {
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

    val (path, cost) = AStarSearch.search('s, succ _, {(_:Symbol) == 'g}, h).get
    val syms = path.toStateList
    assert(syms === List('s, 'd, 'e, 'g))
    assert(cost === 7.0)
  }

}
