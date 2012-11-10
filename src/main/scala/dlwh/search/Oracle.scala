package dlwh.search

/**
 * 
 * @author dlwh
 */
trait Oracle[T] {
  def size: Int

  def accepts(t: T, cost: Double):Boolean
}

object Oracle {
  trait Factory[T] {
    def make: Oracle[T]
  }
}
