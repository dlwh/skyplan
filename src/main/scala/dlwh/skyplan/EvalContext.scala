package dlwh.skyplan

trait EvalContext { outer =>
  /**
   * Returns the ground number of the object referenced by the
   * i'th local varioable. Maintained as a stack, basically.
   * @param i
   * @return
   */
  def local(i: Int):Int
  def resource(fn: Int, args: IndexedSeq[Int]): Double
  def numLocals: Int


  def updateResource(fn: Int, args: IndexedSeq[Int], v: Double)

  def addLocals(bindings: Array[Int]):EvalContext = new EvalContext {
    /**
     * Returns the ground number of the
     * @param i
     * @return
     */
    def local(i: Int): Int = {
      if(i < outer.numLocals) {
        outer.local(i)
      } else {
        bindings(i - outer.numLocals)
      }
    }


    def numLocals: Int = outer.numLocals + bindings.length

    def resource(fn: Int, args: IndexedSeq[Int]): Double = outer.resource(fn, args)

    def updateResource(fn: Int, args: IndexedSeq[Int], v: Double) {
      outer.updateResource(fn, args, v)

    }

    def updateCell(fn: Int, args: IndexedSeq[Int], v: Int) {
      outer.updateResource(fn, args, v)
    }
  }
}

object EvalContext {
  def onlyLocals(lcls: IndexedSeq[Int]):EvalContext = new EvalContext {
    /**
     * Returns the ground number of the object referenced by the
     * i'th local varioable. Maintained as a stack, basically.
     * @param i
     * @return
     */
    def local(i: Int): Int = lcls(i)

    def resource(fn: Int, args: IndexedSeq[Int]): Double = 0.0

    def numLocals: Int = lcls.length

    def updateResource(fn: Int, args: IndexedSeq[Int], v: Double) { sys.error("Not allowed!")}
  }

  val emptyContext:EvalContext = onlyLocals(IndexedSeq.empty)

}
