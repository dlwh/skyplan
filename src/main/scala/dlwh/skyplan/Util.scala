package dlwh.skyplan

/**
 * 
 * @author dlwh
 */
object Util {
  /**
   * Returns all possible sequences, where each element in each iterable can appear in that position.
   *
   * E.g.
   *
   * allArguumentListsForChoices(IndexedSeq(IndexedSeq(3,4),IndexedSeq(5,6))) ==>
   * IndexedSeq(IndexedSeq(3,5),IndexedSeq(3,6),IndexedSeq(4,5), IndexedSeq(4, 6))
   * @param seq
   * @return
   */
  def allArgumentListsForChoices(seq: IndexedSeq[Iterable[Int]]): IndexedSeq[IndexedSeq[Int]] = {
    seq.foldLeft(IndexedSeq(IndexedSeq.empty[Int])){ (acc, objs) =>
      for(a <- acc; i <- objs) yield {
        a :+ i
      }
    }
  }
}
