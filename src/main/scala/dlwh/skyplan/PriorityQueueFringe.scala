package dlwh.skyplan

import collection.mutable

/**
 * Pops elements with the *highest* priority
 * @author dlwh
 */
class PriorityQueueFringe[T] extends Fringe[T] {
  val queue = new mutable.PriorityQueue[(T,Double)]()(Ordering[Double].on(_._2))
  /**
   * Returns true if actually added to fringe
   * @param t the state
   * @return
   */
  def enqueue(t: T, cost: Double): Boolean = {queue += (t -> cost); true}

  def dequeue() = if(isEmpty) None else Some(queue.dequeue())
  def peek() = if(isEmpty) None else Some(queue.head)


  def isEmpty: Boolean = queue.isEmpty
}
