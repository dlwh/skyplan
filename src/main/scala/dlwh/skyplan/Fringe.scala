package dlwh.skyplan

/**
 * 
 * @author dlwh
 */
trait Fringe[T] {
  /**
   * Returns true if actually added to fringe
   * @param t the state
   * @return
   */
  def enqueue(t: T, cost: Double):Boolean
  def dequeue():Option[(T, Double)]

  def isEmpty: Boolean
  def peek():Option[(T, Double)]
}

