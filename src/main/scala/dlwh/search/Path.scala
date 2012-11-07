package dlwh.search

sealed trait Path[+T, +Action] {

  def head: T
  def actionOpt: Option[Action]

  def toStateList: List[T] =  this match {
    case End(head) => List(head)
    case Link(head, _, rest) => head :: rest.toStateList
  }

  def prepend[U >: T, A >: Action](state: U, a: A):Path[U, A] = {
    Link(state, a, this)
  }

  def reverse: Path[T, Action] = {
    def rec(cur: Path[T, Action], a: Action, prev: Path[T, Action]):Path[T, Action] = {
      cur match {
        case End(head) => Link(head, a, prev)
        case Link(h, ma, rest) => rec(rest, a, Link(h, ma, prev))
      }
    }

    this match {
      case End(head) => this
      case Link(h, ma, rest) => rec(rest, ma, End(h))
    }
  }

  def map[U](f: T=>U):Path[U, Action] = this match {
    case End(head) => End(f(head))
    case Link(head, action, rest) => Link(f(head), action, rest.map(f))
  }
}

case class End[T](head: T) extends Path[T, Nothing] {
  def actionOpt = None
}

case class Link[+T, +Action](head: T, action: Action, rest: Path[T, Action]) extends Path[T, Action] {
  def actionOpt: Option[Action] = Some(action)
}
