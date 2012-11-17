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

  def allAllowed[T] = new Factory[T] {
    def make: Oracle[T] = new Oracle[T] {
      var size = 0

      def accepts(t: T, cost: Double) = {size += 1; true}
    }
  }
}

