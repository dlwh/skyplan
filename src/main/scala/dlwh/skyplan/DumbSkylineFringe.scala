package dlwh.skyplan

import collection.mutable.ArrayBuffer
import collection.mutable

/**
 * 
 * @author dlwh
 */
class DumbSkylineFringe[T:PartialOrdering](inner: Fringe[T]) extends Fringe[T] {
  private def order  = implicitly[PartialOrdering[T]]
  /**
   * Returns true if actually added to fringe
   * @param t the state
   * @return
   */
  def enqueue(t: T, cost: Double): Boolean = {
    checkDominance(t) match {
      case Left(dominator) =>
        false
      case Right(dominated) =>
        elements --= dominated
        kickedOut ++= dominated
        inner.enqueue(t, cost)
        true
    }
  }

  private var next: Option[(T, Double)] = None


  def isEmpty: Boolean = {
    if(next != None) false
    else {
      next = peek()
      if(next == None) true
      else false
    }
  }

  def peek(): Option[(T, Double)] = {
    while(next.isEmpty && !inner.isEmpty) {
      next = inner.peek()
      if(kickedOut(next.get._1)) {
        inner.dequeue()
        next = None
      }
    }
    next
  }


  def dequeue(): Option[(T, Double)] = {
    peek()
    inner.dequeue()
    val n = next
    next = None
    n
  }

  // Returns either the element that dominates elem, or a list of things it dominates
  private def checkDominance(elem: T):Either[T, Seq[T]] = {
    val newlyKickedOut = new ArrayBuffer[T]()
    for(i <- elements) {
      order.tryCompare(elem, i) match {
        case None => // nothing
        case Some(v) =>
          if(v <= 0) {
            assert(elements.isEmpty)
            return Left(i)
          } else if(v > 0) {
            newlyKickedOut += i
          }
      }
    }

    Right(newlyKickedOut)
  }


  private val elements = new mutable.HashSet[T]()
  private val kickedOut = new mutable.HashSet[T]()
}
