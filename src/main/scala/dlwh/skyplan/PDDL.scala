package dlwh.skyplan

import java.io.StringReader

/**
 * Data representation and parsing for PDDL.
 * @author dlwh
 */
object PDDL {

  def parseDomain(str: String):Domain = {
    import DomainReader._
    parse(phrase(DomainReader.domain), new StringReader(str)) match {
      case Success(dom, _) => dom
      case Failure(msg, _) => throw new RuntimeException("Parse error: " + msg)
    }
  }

  def parseProblem(str: String):Problem = {
    import DomainReader._
    parse(phrase(DomainReader.problem), new StringReader(str)) match {
      case Success(prob, _) => prob
      case Failure(msg, _) => throw new RuntimeException("Parse error: " + msg)
    }
  }

  case class Domain(name: String,
                    requirements: Set[String] = Set.empty,
                    types: IndexedSeq[Type] = IndexedSeq.empty,
                    constants: IndexedSeq[Argument] = IndexedSeq.empty,
                    predicates: Map[String, Predicate] = Map.empty,
                    functions: Map[String, Function] = Map.empty,
                    constraints: IndexedSeq[Constraint] = IndexedSeq.empty,
                    actions: Map[String, Action] = Map.empty
                     )

  case class Problem(name: String,
                     domainName: String,
                     requirements: Set[String] = Set.empty,
                     objects: Set[Argument] =  Set.empty,
                     initialState: Effect = AndEffect(IndexedSeq.empty),
                     goal: Condition = AndCondition(IndexedSeq.empty),
                     metric: Option[MetricSpec] = None)

  case class MetricSpec(m: MetricDir, value: ValExp)
  sealed trait MetricDir
  case object Minimize extends MetricDir
  case object Maximize extends MetricDir


  case class Type(name: String, parent: String)
  case class Argument(name: String, tpe: String)
  case class Predicate(name: String, args: IndexedSeq[Argument])
  case class Function(name: String, args: IndexedSeq[Argument], resultType: String = "number")
  case class Constraint()

  case class Action(name: String,
                    args: IndexedSeq[Argument],
                    precondition: Option[Condition],
                    effect: Option[Effect],
                    duration: Option[DurationConstraint] = None)

  sealed trait Condition
  case class AndCondition(conjuncts: IndexedSeq[Condition]) extends Condition
  case class UniversalCondition(scoped: IndexedSeq[Argument], cond: Condition) extends Condition
  case class ExistentialCondition(scoped: IndexedSeq[Argument], cond: Condition) extends Condition
  case class FComp(comp: BinaryComp, arg1: ValExp, arg2: ValExp) extends Condition
  case class RComp(arg1: RefExp, arg2: RefExp) extends Condition
  case class Pred(predicate: String, args: IndexedSeq[RefExp]) extends Condition
  case class TimedCondition(time: TimeSpecifier, cond: Condition) extends Condition
  case class ContinuousCondition(cond: Condition) extends Condition

  sealed trait ValExp
  sealed trait RefExp
  case class Name(name: String) extends RefExp
  case class Var(name: String) extends RefExp
  case class RApplication(name: String, args: IndexedSeq[RefExp] = IndexedSeq.empty) extends RefExp

  case object DurationReference extends ValExp
  case class FApplication(name: String, args: IndexedSeq[RefExp] = IndexedSeq.empty) extends ValExp
  case class Number(num: Double) extends ValExp
  case class BinaryExp(op: BinaryOp, lhs: ValExp, rhs: ValExp) extends ValExp
  case class MultiExp(op: MultiOp, args: IndexedSeq[ValExp]) extends ValExp
  case class Negation(x: ValExp) extends ValExp

  sealed trait BinaryComp {
    def apply(a: Double, b: Double):Boolean
  }
  case object < extends BinaryComp {
    def apply(a: Double, b: Double) = a < b
  }
  case object > extends BinaryComp {
    def apply(a: Double, b: Double) = a > b
  }
  case object <= extends BinaryComp {
    def apply(a: Double, b: Double) = a <= b
  }
  case object >= extends BinaryComp {
    def apply(a: Double, b: Double) = a >= b
  }
  case object Equals extends BinaryComp {
    def apply(a: Double, b: Double) = a == b
    def apply(a: Int, b: Int) = a == b
  }

  sealed trait Effect
  case class AndEffect(conjuncts: IndexedSeq[Effect]) extends Effect
  case class UniversalEffect(scoped: IndexedSeq[Argument], effect: Effect) extends Effect
  case class CondEffect(condition: Condition, effect: Effect) extends Effect
  case class TimedEffect(time: TimeSpecifier, effect: Effect) extends Effect

  sealed trait PrimEffect extends Effect
  case class DisablePred(pred: Pred) extends PrimEffect
  case class EnablePred(pred: Pred) extends PrimEffect
  case class RefAssignEffect(lhs: RApplication, rhs: RefExp) extends PrimEffect
  case class AssignEffect(op: AssignOp, lhs: FApplication, rhs: ValExp) extends PrimEffect {
    def toRefAssignEffect = {
      require(op == Assign, "Can't " + op + " references!")
      val rexp = rhs match {
        case FApplication(name, args) => RApplication(name, args)
        case _ => throw new RuntimeException("Type Error: Can't assign to a reference from a " + rhs)
      }
      RefAssignEffect(RApplication(lhs.name, lhs.args), rexp)
    }
  }

  sealed trait TimeSpecifier
  case object Start extends TimeSpecifier
  case object End extends TimeSpecifier




  sealed trait DurationConstraint
  case class StandardDuration(comp: BinaryComp, value: ValExp) extends DurationConstraint


  import scala.util.parsing.combinator.{ RegexParsers, JavaTokenParsers }


  object DomainReader extends RegexParsers with JavaTokenParsers with LispTokens {
    lazy val domain:Parser[Domain] = fn("define")(
      (fn("domain")(name) ^^ {Domain(_)})
        ~ rep[Domain=>Domain](
        requirements ^^ {r => (d: Domain) => d.copy(requirements=r.toSet)}
          | types ^^ {r => (d: Domain) => d.copy(types=r)}
          | constants ^^ {r => (d: Domain) => d.copy(constants=r)}
          | predicates ^^ {r => (d: Domain) => d.copy(predicates=r)}
          | functions ^^ {r => (d: Domain) => d.copy(functions=r)}
          // TODO constraints
          | action ^^ {r => (d: Domain) => d.copy(actions=d.actions + (r.name -> r))}
      )
    ) ^^ { case (domain ~ fns) =>
      fns.foldLeft(domain)((d,f) => f(d))
    }


    lazy val types = field("types")(typeList)
    lazy val constants = field("constants")(argList)
    lazy val predicates = field("predicates")(rep(predicate)) ^^ {_.toMap}
    lazy val functions = field("functions")(typedList(function, addResultType _, "number") ) ^^ {functions => functions.map(f => f.name -> f).toMap}
    lazy val action = basicAction | durativeAction



    lazy val predicate = surround(name ~ varList) ^^ { case (name ~ args) => name -> Predicate(name, args)}
    lazy val function = surround(name ~ varList) ^^ { case (name ~ args) => Function(name, args)}
    lazy val basicAction = field("action")(
        name
        ~ (":parameters" ~> commit(surround(varList)))
        ~ (opt(precondition) ^^ { (option: Option[Option[Condition]]) => option.flatMap(identity _)})
        ~ opt(sym("effect") ~> commit(effect))
     ) ^^ { case name ~  args ~ precondition ~ effect => Action(name, args, precondition, effect)}

    lazy val durativeAction = field("durative-action")(
        name
        ~ (":parameters"~>(surround(varList)))
        ~ (":duration" ~> (duration_constraint))
        ~ (opt(condition) ^^ { (option: Option[Option[Condition]]) => option.flatMap(identity _)})
        ~ opt(sym("effect") ~> commit(effect))
     ) ^^ { case name ~ args ~ duration ~ precondition ~ effect => Action(name, args, precondition, effect, duration)}



    lazy val precondition: Parser[Option[Condition]]  = sym("precondition") ~> emptyOr(pre_gd)

    lazy val effect:Parser[Effect] = (
       surround("=" ~ fterm(RApplication(_,_)) ~ term) ^^ { case a ~ b ~ c=> RefAssignEffect(b,c)}
         // note that this assignment can be
      | surround(assign_op ~ commit(fterm(FApplication(_,_))) ~ fexp) ^^ { case a ~ b ~ c=> AssignEffect(a,b,c)}
      | fn("and")(rep(effect)) ^^ {list => AndEffect(list.toIndexedSeq)}
      | fn("forall")(varList ~ effect) ^^ { case  list~eff => UniversalEffect(list, eff)}
      | fn("when")(gd ~ effect) ^^ { case  gd~eff => CondEffect(gd, eff)}
      | fn("not")(atomic_pred) ^^ {DisablePred(_)}
      | timed_effect
      | atomic_pred ^^ {EnablePred(_)}
    )

    lazy val timed_effect = (
      fn("at")(time_spec ~ commit(effect)) ^^ { case (ts ~ eff) => TimedEffect(ts, eff)}
      )


    lazy val assign_op = (
      "=" ^^ { _ => Assign}
      | "assign" ^^ { _ => Assign}
        | "scale-up" ^^ { _ => ScaleUp}
        | "scale-down" ^^ { _ => ScaleDown}
        | "increase" ^^ { _ => Increase}
        | "decrease" ^^ { _ => Decrease}
      )

    lazy val pre_gd:Parser[Condition] = (
      surround("and" ~> rep(pre_gd)) ^^ {res => AndCondition(res.toIndexedSeq)}
        | surround("forall" ~> surround(argList) ~ pre_gd) ^^ { case (vars ~ gd) => UniversalCondition(vars, gd)}
        | gd
      )

    lazy val gd: Parser[Condition] =  (
      timed_gd
      | fn("exists")(surround(varList) ~ gd) ^^ {case (vars ~ con) => ExistentialCondition(vars, con)}
      | fn("forall")(surround(varList) ~ gd) ^^ {case (vars ~ con) => UniversalCondition(vars, con)}
      | rcomp
      | fcomp
      | atomic_pred
    )

    lazy val timed_gd: Parser[Condition] = (
      fn("at")(time_spec ~ commit(gd)) ^^ { case (ts ~ gd) => TimedCondition(ts, gd)}
        | fn("over")("all" ~> gd) ^^ { ContinuousCondition(_)}
      )

    lazy val time_spec = (
      "start" ^^^ {Start}
        | "end" ^^^ {End}
      )

    lazy val atomic_pred = surround(name ~ rep(term)) ^^ {case (name ~ args) => Pred(name, args.toIndexedSeq)}

    lazy val term: Parser[RefExp] = (
      name ^^ {Name(_)}
      | variable ^^ {Var(_)}
      | fterm(RApplication(_, _))
      )

    lazy val fexp:Parser[ValExp] = (
      number
        | surround(multi_op ~ rep1(fexp)) ^^ {case (a ~ b) => MultiExp(a,b.toIndexedSeq)}
        | surround(binary_op ~ fexp ~ fexp) ^^ {case (a ~ b ~ c) => BinaryExp(a,b,c)}
      | '-' ~> fexp ^^ { Negation(_)}
      | fterm(FApplication(_,_))
      | duration_ref
    )

    lazy val duration_constraint = emptyOr(simple_duration_constraint)
    lazy val simple_duration_constraint = {
      surround(binary_comp ~ duration_ref ~ fexp) ^^ {case op ~ _ ~ fexp => StandardDuration(op, fexp)}
    }

    lazy val duration_ref = "?duration" ^^^  DurationReference

    // problem stuff
    lazy val problem = fn("define")(
      (fn("problem")(name) ~ field("domain")(name) ^^ {case nme~dom => Problem(nme, dom)})
      ~ rep(
        requirements ^^ {r => (d: Problem) => d.copy(requirements=r.toSet)}
      | (field("objects")(objects) ^^ {r => (d: Problem) => d.copy(objects = r.toSet ++ d.objects)})
      | (field("init")(init) ^^ {r => (d: Problem) => d.copy(initialState = r)})
      | (field("goal")(goal) ^^ {r => (d: Problem) => d.copy(goal=r)})
      | (field("metric")(metric_spec) ^^ {r => (d: Problem) => d.copy(metric=Some(r))})
      ) ^^ { case prob ~ fs => fs.foldLeft(prob)((p,f) => f(p))}
    )

    lazy val objects = typedList(name, Argument(_:String, _:String))
    lazy val init = rep(init_el) ^^ {l => AndEffect(l.toIndexedSeq)}
    lazy val goal = pre_gd
    lazy val metric_spec = metric_dir ~ fexp ^^ {case (a ~ b) => MetricSpec(a, b)}

    lazy val metric_dir = ("minimize" ^^^ Minimize | "maximize" ^^^ Maximize)


    lazy val init_el = effect




    // common stuff
    lazy val condition: Parser[Option[Condition]]  = sym("condition") ~> emptyOr(pre_gd)

    lazy val binary_comp = (
      "<=" ^^ { _ => <= }
        | ">=" ^^ { _ => >= }
        | ">" ^^ { _ => > }
        | "<" ^^ { _ => < }
        | "=" ^^ { _ => Equals }
      )

    lazy val binary_op = (
      "-" ^^ { _ => Minus }
        | "/" ^^ { _ => Div }
        | multi_op
      )

    lazy val multi_op = (
      "*" ^^ { _ => Times}
      | "+" ^^ { _ => Plus}
      )


    def fterm[T](fn: (String,IndexedSeq[RefExp])=>T) = surround(name ~ rep(term)) ^^ { case (name~ args) => fn(name, args.toIndexedSeq)}

    lazy val fcomp = surround(binary_comp ~ fexp ~ fexp) ^^ { case (b~l~r) => FComp(b, l, r)}
    lazy val rcomp = surround("=" ~ term ~ term) ^^ { case (b~l~r) => RComp(l, r)}

    lazy val argList = typedList(name, Argument(_:String, _: String))
    lazy val varList = typedList(variable, Argument(_:String, _:String))
    lazy val typeList = typedList(variable, Type(_:String, _: String))


    def typedList[T, U](vp: Parser[T], lift: (T,String)=>U, defaultType: String="object"): Parser[IndexedSeq[U]] = {
      rep(rep1(vp) ~ opt("-" ~> name) ^^ {case (v ~ t) => v.map(lift(_,t.getOrElse(defaultType))).toIndexedSeq}) ^^ {_.toIndexedSeq.flatten}
    }


    def addResultType(f: Function, name: String):Function = f.copy(resultType = name)


  }





  trait LispTokens { this: RegexParsers =>
    val name:Parser[String] =  """[a-zA-Z_@~%!=#<>\+\^\&\-][0-9a-zA-Z_@~%!=#<>\+\*\^\&\-]*""".r
    val variable:Parser[String] =  "?" ~> name
    val number:Parser[Number] =  "[0-9]+".r ^^ { x => Number(x.replace("\\.","").toDouble)}
    lazy val lParen: Parser[String] = "("
    lazy val rParen: Parser[String] = ")"
    lazy val lBrack: Parser[String] = "["
    lazy val rBrack: Parser[String] = "]"
    lazy val quote: Parser[String] = "'"
    def sym(string: Parser[String]):Parser[String] = ":" ~> string

    def emptyOr[T](parser: Parser[T]): Parser[Option[T]] = lParen ~ rParen ^^ { _ => None } | parser ^^ {Some(_)}
    def surround[T](parser: Parser[T]) = lParen ~> parser <~ rParen
    def field[T, U](name: String)(arg: Parser[U]) = surround(sym(name) ~> commit(arg))
    def fn[U](name: String)(arg: Parser[U]) = surround(name ~> arg)

    lazy val requirements = field("requirements")(rep(sym(name)))
  }

}
