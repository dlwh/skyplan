package dlwh.skyplan

import breeze.util.Index
import PDDL._

trait EvalContext { outer =>
  /**
   * Returns the ground number of the object referenced by the
   * i'th local varioable. Maintained as a stack, basically.
   * @param i
   * @return
   */
  def local(i: Int):Int
  def resource(fn: Int, args: IndexedSeq[Int]): Double
  def cell(fn: Int, args: IndexedSeq[Int]): Int

  def numLocals: Int


  def updateResource(fn: Int, args: IndexedSeq[Int], v: Double)
  def updateCell(fn: Int, args: IndexedSeq[Int], v: Int)

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

    def cell(fn: Int, args: IndexedSeq[Int]): Int = outer.cell(fn, args)

    def updateResource(fn: Int, args: IndexedSeq[Int], v: Double) {
      outer.updateResource(fn, args, v)

    }

    def updateCell(fn: Int, args: IndexedSeq[Int], v: Int) {
      outer.updateResource(fn, args, v)
    }
  }
}

sealed trait CellExpression {
  def cell(context: EvalContext):Int
}


sealed trait ValExpression {
  def valueWith(context: EvalContext):Double
}

import Expression._

object CellExpression {
  def fromRefExp(term: RefExp,
               refFunctions: Index[String],
               locals: Index[String],
               globals: Index[String]):CellExpression = term match {
    case Name(x) =>
      val r = globals(x)
      if (r < 0) throw new ExpressionException("Unknown name " + x + " " + globals)
      Global(r, x)
    case Var(x) =>
      val r = locals(x)
      if (r < 0) throw new ExpressionException("Unknown argument " + x)
      Local(r, x)
    case RApplication(name, args) =>
      val fn = refFunctions(name)
      if (fn < 0) throw new ExpressionException("Unknown function " + fn)
      Cell(fn, args.map(fromRefExp(_, refFunctions, locals, globals)))
  }
}

object Expression {

  def fromValExp(fexp: ValExp,
               refFunctions: Index[String],
               valFunctions: Index[String],
               locals: Index[String],
               globals: Index[String]):ValExpression = fexp match {
    case FApplication(name, args) =>
      val fn = valFunctions(name)
      if (fn < 0) throw new ExpressionException("Unknown function " + name)
      Resource(fn, args.map(CellExpression.fromRefExp(_, refFunctions, locals, globals)))
    case BinaryExp(op, lhs, rhs) =>
      Binary(op, fromValExp(lhs, refFunctions, valFunctions, locals, globals), fromValExp(rhs, refFunctions, valFunctions, locals, globals))
    case MultiExp(op, args) =>
      Multi(op, args.map(fromValExp(_, refFunctions, valFunctions, locals, globals)))
    case PDDL.Number(num) =>
      Number(num)
    case PDDL.Negation(arg) =>
      Negation(fromValExp(arg, refFunctions, valFunctions, locals, globals))
  }


  case class Global(x: Int, name: String) extends CellExpression {
    def cell(context: EvalContext) = x
  }

  case class Local(x: Int, name: String) extends CellExpression {
    def cell(context: EvalContext) = try {
      context.local(x)
    } catch {
      case e: IndexOutOfBoundsException => throw new RuntimeException("bad indexing?!?!" + x + " " + name, e)
    }
  }

  case class Number(x: Double) extends ValExpression {
    def valueWith(context: EvalContext): Double = x
  }

  case class Resource(fn: Int, args: IndexedSeq[CellExpression]) extends ValExpression {
    def valueWith(context: EvalContext) = context.resource(fn, args.map(a => a.cell(context)))
    def update(context: EvalContext, v: Double) = context.updateResource(fn, args.map(a => a.cell(context)), v)
  }

  case class Cell(fn: Int, args: IndexedSeq[CellExpression]) extends CellExpression {
    def cell(context: EvalContext): Int = context.cell(fn, args.map(a => a.cell(context)))

    def update(context: EvalContext, v: Int) = context.updateCell(fn, args.map(a => a.cell(context)), v)

  }

  object Cell {
    def fromRApplication(r: RApplication) {

    }
  }

  case class Binary(op: BinaryOp, lhs: ValExpression, rhs: ValExpression) extends ValExpression {
    def valueWith(context: EvalContext): Double = op(lhs.valueWith(context),rhs.valueWith(context))
  }

  case class Multi(op: MultiOp, args: IndexedSeq[ValExpression]) extends ValExpression {

    def valueWith(context: EvalContext) = op(args.map(a => a.valueWith(context)))
  }

  case class Negation(arg: ValExpression) extends ValExpression {
    def valueWith(context: EvalContext) = -arg.valueWith(context)
  }

  case class ExpressionException(msg: String) extends Exception(msg)
}




sealed trait BinaryOp {
  def apply(x: Double, y: Double):Double
}
sealed trait MultiOp extends BinaryOp {
  def apply(x: IndexedSeq[Double]):Double
}
case object Minus extends BinaryOp {
  def apply(x: Double, y: Double):Double = x - y
}
case object Div extends BinaryOp {
  def apply(x: Double, y: Double):Double = x / y
}
case object Times extends MultiOp {
  def apply(x: Double, y: Double):Double = x * y
  def apply(x: IndexedSeq[Double]) = x.product
}

case object Plus extends MultiOp {
  def apply(x: Double, y: Double):Double = x + y
  def apply(x: IndexedSeq[Double]) = x.sum
}

