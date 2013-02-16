package dlwh.skyplan

import breeze.util.Index
import PDDL._
import collection.immutable.BitSet

sealed trait CellExpression {
  def cell(context: EvalContext):Int
}


sealed trait ValExpression {

  def valueWith(context: EvalContext):Double

  /**
   * Tries to see if this expression always has a constant sign.
   * @param context
   * @return None if the sign could be positive, negative, or zero,
   *         Some(1) if it's only non-negative
   *         Some(-1) if it's only negative
   *         Some(0) if it's always zero.
   */
  def possibleSignWithArgs(context: EvalContext):Option[Int]

  /**
   * returns which resources have positive or negative sign
   * could be both
   * @param context
   * @return those with positive sign, those with negative sign
   */
  def getResourceSigns(inst: ProblemInstance, context: IndexedSeq[Int]): (BitSet, BitSet)
}

import Expression._

object CellExpression {
  def fromRefExp(term: RefExp,
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
  }
}

object Expression {

  def fromValExp(fexp: ValExp,
               resources: Index[String],
               constResources: Index[String],
               locals: Index[String],
               globals: Index[String]):ValExpression = fexp match {
    case FApplication(name, args) =>
      val fn = resources(name)
      if (fn < 0) {
        val fn2 = constResources(name)
        if(fn2 < 0)
          throw new ExpressionException("Unknown function " + name)
        ConstResource(fn2, args.map(CellExpression.fromRefExp(_, locals, globals)))
      } else {
        Resource(fn, args.map(CellExpression.fromRefExp(_, locals, globals)))
      }
    case BinaryExp(op, lhs, rhs) =>
      Binary(op, fromValExp(lhs, resources, constResources, locals, globals), fromValExp(rhs, resources, constResources, locals, globals))
    case MultiExp(op, args) =>
      Multi(op, args.map(fromValExp(_, resources, constResources, locals, globals)))
    case PDDL.Number(num) =>
      Number(num)
    case PDDL.Negation(arg) =>
      Negation(fromValExp(arg, resources, constResources, locals, globals))
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

    def possibleSignWithArgs(context: EvalContext): Option[Int] = Some(math.signum(x).toInt)


    def getResourceSigns(inst: ProblemInstance, context: IndexedSeq[Int]): (BitSet, BitSet) = BitSet.empty -> BitSet.empty
  }

  case class Resource(fn: Int, args: IndexedSeq[CellExpression]) extends ValExpression {
    def valueWith(context: EvalContext) = context.resource(fn, args.map(a => a.cell(context)))
    def update(context: EvalContext, v: Double) = context.updateResource(fn, args.map(a => a.cell(context)), v)
    def possibleSignWithArgs(context: EvalContext): Option[Int] = None

    def possibleGroundings(inst: ProblemInstance, locals: IndexedSeq[Int]):IndexedSeq[Int] = {
      val argumentChoices = args.map {
        case Global(v, _) => IndexedSeq(v)
        case Local(v, _) => IndexedSeq(locals(v))
      }
      Util.allArgumentListsForChoices(argumentChoices).map(inst.valFuns.ground(fn, _))
    }

    /**
     * returns which resources have positive or negative sign
     * could be both
     * @param context
     * @return those with positive sign, those with negative sign
     */
    def getResourceSigns(inst: ProblemInstance, context: IndexedSeq[Int]): (BitSet, BitSet) = {
      (BitSet.empty ++ possibleGroundings(inst, context), BitSet.empty)
    }
  }

  case class ConstResource(fn: Int, args: IndexedSeq[CellExpression]) extends ValExpression {
    assert(fn >= 0, fn)
    def valueWith(context: EvalContext) = context.constResource(fn, args.map(a => a.cell(context)))
    def update(context: EvalContext, v: Double) = error("Can't update constant resource!")
    def possibleSignWithArgs(context: EvalContext): Option[Int] = None

    def possibleGroundings(inst: ProblemInstance, locals: IndexedSeq[Int]):IndexedSeq[Int] = {
      val argumentChoices = args.map {
        case Global(v, _) => IndexedSeq(v)
        case Local(v, _) => IndexedSeq(locals(v))
      }
      Util.allArgumentListsForChoices(argumentChoices).map(inst.valConstants.ground(fn, _))
    }

    def getResourceSigns(inst: ProblemInstance, context: IndexedSeq[Int]): (BitSet, BitSet) = {
      (BitSet.empty, BitSet.empty)
    }
  }


  case class Binary(op: BinaryOp, lhs: ValExpression, rhs: ValExpression) extends ValExpression {
    def valueWith(context: EvalContext): Double = op(lhs.valueWith(context),rhs.valueWith(context))
    def possibleSignWithArgs(context: EvalContext): Option[Int] = op match {
      case Minus =>
        for(left <- lhs.possibleSignWithArgs(context);
            right <- rhs.possibleSignWithArgs(context) if left != right) yield {
          if(left > right) left else right
        }
      case Plus =>
        for(left <- lhs.possibleSignWithArgs(context);
            right <- rhs.possibleSignWithArgs(context) if left == right || left == 0 || right == 0) yield {
          math.signum(left + right)
        }
      case Times =>
        for(left <- lhs.possibleSignWithArgs(context);
            right <- rhs.possibleSignWithArgs(context)) yield {
          left * right
        }
      case Div =>
        for(left <- lhs.possibleSignWithArgs(context);
            right <- rhs.possibleSignWithArgs(context)) yield {
          left * right
        }
    }

    /**
     * returns which resources have positive or negative sign
     * could be both
     * @param context
     * @return those with positive sign, those with negative sign
     */
    def getResourceSigns(inst: ProblemInstance, context: IndexedSeq[Int]): (BitSet, BitSet) = op match {
      case Minus =>
        val (plhs, nlhs) = lhs.getResourceSigns(inst, context)
        val (prhs, nrhs) = rhs.getResourceSigns(inst, context)
        (plhs ++ nrhs)  -> (nlhs ++ prhs)
      case Plus =>
        val (plhs, nlhs) = lhs.getResourceSigns(inst, context)
        val (prhs, nrhs) = rhs.getResourceSigns(inst, context)
        (plhs ++ prhs)  -> (nlhs ++ nrhs)
      case Times =>
        val l@(plhs, nlhs) = lhs.getResourceSigns(inst, context)
        val r@(prhs, nrhs) = rhs.getResourceSigns(inst, context)
        val rhsSign = rhs.possibleSignWithArgs(EvalContext.onlyLocals(context))
        val lhsSign = lhs.possibleSignWithArgs(EvalContext.onlyLocals(context))
        if(prhs.isEmpty && nrhs.isEmpty && rhsSign.nonEmpty) {
          rhsSign.get match {
            case x if x > 0 => l
            case 0 => BitSet.empty -> BitSet.empty
            case x => l.swap
          }
        }
        else if(plhs.isEmpty && nlhs.isEmpty && lhsSign.nonEmpty) {
          lhsSign.get match {
            case x if x > 0 => r
            case 0 => BitSet.empty -> BitSet.empty
            case x => r.swap
          }
        } else {
          val all = plhs ++ nlhs ++ prhs ++ nrhs
          all -> all
        }
      case Div =>
        // fuck it
        val (plhs, nlhs) = lhs.getResourceSigns(inst, context)
        val (prhs, nrhs) = rhs.getResourceSigns(inst, context)
        val all = plhs ++ nlhs ++ prhs ++ nrhs
        all -> all
    }
  }

  case class Multi(op: MultiOp, args: IndexedSeq[ValExpression]) extends ValExpression {

    def valueWith(context: EvalContext) = op(args.map(a => a.valueWith(context)))

    def possibleSignWithArgs(context: EvalContext): Option[Int] = op match {
      case Plus =>
        val signs = args.map(_.possibleSignWithArgs(context))
        if(signs.exists(_.isEmpty)) None
        else {
          var seenPos: Boolean = false
          var seenNeg: Boolean = false
          for(Some(s) <- signs) {
            if(s > 0) seenPos = true
            else if(s < 0) seenNeg = true
            if(seenPos && seenNeg) return None
          }
          if(seenPos) Some(1)
          else if(seenNeg) Some(-1)
          else Some(0)
        }
      case Times =>
        val signs = args.map(_.possibleSignWithArgs(context))
        if(signs.exists(_.isEmpty)) None
        else {
          var sign = 1
          for(Some(s) <- signs) {
            if(s == 0) return Some(0)
            sign *= s
          }
          Some(sign)
        }
    }

    /**
     * returns which resources have positive or negative sign
     * could be both
     * @param context
     * @return those with positive sign, those with negative sign
     */
    def getResourceSigns(inst: ProblemInstance, context: IndexedSeq[Int]): (BitSet, BitSet) = op match {
      case Plus =>
        val (pos, neg) = args.map(_.getResourceSigns(inst, context)).unzip
        pos.reduceLeft(_ ++ _) -> neg.reduceLeft(_ ++ _)
      case Times =>
        val (constants, other) = args.partition(_.isInstanceOf[Number])
        val sign = math.signum(constants.map(_.asInstanceOf[Number].x).product)

        if(other.size == 0) {
          BitSet.empty -> BitSet.empty
        } else if(other.size == 1) {
          if(sign < 0) (other(0).getResourceSigns(inst, context)).swap
          else if(sign == 0) BitSet.empty -> BitSet.empty
          else (other(0).getResourceSigns(inst, context))
        } else { // punt on signs if we multiply more than two fluents together.
          val (pos, neg) = other.map(_.getResourceSigns(inst, context)).unzip
          val all = pos.reduce(_ ++ _) ++ neg.reduce(_ ++ _)
          all -> all
        }
    }
  }

  case class Negation(arg: ValExpression) extends ValExpression {
    def valueWith(context: EvalContext) = -arg.valueWith(context)

    /**
     * Tries to see if this expression always has a constant sign.
     * @param context
     * @return None if the sign could be positive, negative, or zero,
     *         Some(1) if it's only non-negative
     *         Some(-1) if it's only negative
     *         Some(0) if it's always zero.
     */
    def possibleSignWithArgs(context: EvalContext): Option[Int] = arg.possibleSignWithArgs(context).map(- _)

    def getResourceSigns(inst: ProblemInstance, context: IndexedSeq[Int]): (BitSet, BitSet) = arg.getResourceSigns(inst, context).swap
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

