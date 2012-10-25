package dlwh.skyplan

import breeze.collection.mutable.OpenAddressHashArray
import collection.mutable
import collection.mutable.ArrayBuffer
import breeze.util.Encoder

/**
 * Created with IntelliJ IDEA.
 * User: dburkett
 * Date: 10/16/12
 * Time: 3:50 PM
 * To change this template use File | Settings | File Templates.
 */
class ActionQueue(actions: Grounding[IndexedAction]) {
  val data : OpenAddressHashArray[mutable.Queue[Double]] = new OpenAddressHashArray[mutable.Queue[Double]](actions.size)
  var nextTime : Double = Double.PositiveInfinity

  def dequeue(time: Double = nextTime): Seq[(Int, Double)] = {
    var newNextTime = Double.PositiveInfinity
    val d = ArrayBuffer[(Int, Double)]()
    for ((a, q) <- data.activeIterator) {
      while (!q.isEmpty && q.head <= time) {
        d += (a -> q.dequeue())
      }
      if (!q.isEmpty) newNextTime = math.min(newNextTime, q.head)
    }
    nextTime = newNextTime
    d
  }

  def enqueue(action: Int, time: Double) {
    if (time < nextTime) nextTime = time
    var q = data(action)
    if (q == null) {
      q = mutable.Queue.empty[Double]
      data(action) = q
    }
    if (q.isEmpty) {
      q.enqueue(time)
      return
    }
    if (q.last > time) {
      val newQ = mutable.Queue.empty[Double]
      while (q.head <= time) {
        newQ.enqueue(q.dequeue())
      }
      newQ.enqueue(time)
      while (!q.isEmpty) {
        newQ.enqueue(q.dequeue())
      }
      data(action) = newQ
    } else {
      q.enqueue(time)
    }
  }

  override def clone = {
    val newAQ = new ActionQueue(actions)
    newAQ.nextTime = nextTime
    for ((a, q) <- data.activeIterator) {
      newAQ.data(a) = q.clone
    }
    newAQ
  }

  override def equals(other: Any) = {
    other match {
      case other: ActionQueue =>
        nextTime == other.nextTime && data.equals(other.data)
      case _ => false
    }
  }

  override def toString: String = {
    data.activeIterator.map{ case (i, v) => actions.groundedIndex.get(i) -> v}.mkString("ActionQueue(nextTime=" + nextTime+ ", queue=[", "," ,"])")
  }
}
