package typechecker

import lambda.Expr
import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, Map}

// Ident for type variables
type Ident = String

private var counter = 0

def freshIdent(): Ident =
  val c = counter
  counter += 1
  s"t$c"

enum MonoType:
  case Var(ident: Ident)
  case Concrete(name: String, args: List[MonoType] = Nil)

object MonoType:
  def concrete(name: String, args: MonoType*): MonoType =
    MonoType.Concrete(name, args.toList)

enum PolyType:
  case Mono(t: MonoType)
  case ForAll(name: Ident, p: PolyType)

given monoToPoly: Conversion[MonoType, PolyType] with
  override def apply(m: MonoType): PolyType = PolyType.Mono(m)

type Context = HashMap[String, PolyType]

case class Substitution(mappings: Map[Ident, MonoType]):
  override def toString: String =

    val sb = StringBuilder()
    sb ++= "Subst{"
    for ((k, v), index) <- mappings.zipWithIndex do
      if index != 0 then
        sb ++= ", "
      sb ++= s"$k: "
      val m = pprint(v)
      sb ++= m
    sb ++= "}"
    sb.toString()

  def apply(monoType: MonoType): MonoType =
    monoType match
      case MonoType.Var(ident) =>
        mappings.getOrElse(ident, monoType)
      case MonoType.Concrete(name, args) =>
        val mappedArgs = args.map(this.apply)
        MonoType.Concrete(name, mappedArgs)

  def apply(polyType: PolyType): PolyType =
    polyType match
      case PolyType.Mono(m) => this(m)
      case PolyType.ForAll(v, m) =>
        PolyType.ForAll(v, this(m))

  def apply(context: Context): Context =
    val entries = context.map((k, v) => (k, this(v))).toSeq
    HashMap(entries*)

  /// Return a substitution that behaves in the same way as applying `this` and then `other`
  def compose(other: Substitution): Substitution =
    val entries = (mappings.keys ++ other.mappings.keys).map(k =>
      val m = other(this(MonoType.Var(k)))
      (k, m)
    )
    Substitution.fromEntries(entries.toSeq*)

object Substitution:
  def fromEntries(entries: (Ident, MonoType)*): Substitution = Substitution(Map(entries *))
  def empty: Substitution = Substitution(HashMap.empty)

enum UnifyError:
  case CannotUnify
  case OccursCheck

@tailrec
def unify(t1: MonoType, t2: MonoType): Either[UnifyError, Substitution] =
  (t1, t2) match
    case (MonoType.Concrete(n1, _), MonoType.Concrete(n2, _)) if n1 != n2 =>
      Left(UnifyError.CannotUnify)

    case (MonoType.Concrete(_, args1), MonoType.Concrete(_, args2)) =>
      unifyTraverse(args1, args2)

    case (MonoType.Var(v1), MonoType.Var(v2)) if v1 == v2 =>
      Right(Substitution.empty)

    case (MonoType.Var(v1), _) if occursCheck(v1, t2) =>
      Left(UnifyError.OccursCheck)

    case (MonoType.Var(v1), _) =>
      Right(Substitution.fromEntries(v1 -> t2))

    case (_, MonoType.Var(v2)) =>
      unify(t2, t1)

private def occursCheck(v: Ident, t: MonoType): Boolean =
  t match
    case MonoType.Var(v2) => v == v2
    case MonoType.Concrete(_, args) =>
      args.exists(occursCheck(v, _))

private def unifyTraverse(args1: List[MonoType], args2: List[MonoType], acc: Substitution = Substitution.empty): Either[UnifyError, Substitution] =
  (args1, args2) match
    case (Nil, Nil) => Right(acc)
    case (_ :: _, Nil) | (Nil, _ :: _) => Left(UnifyError.CannotUnify)
    case (t1 :: tl1, t2 :: tl2) =>
      for sub <- unify(t1=acc(t1), t2=acc(t2))
          sub2 <- unifyTraverse(tl1, tl2, sub)
      yield sub2 compose sub

def instantiate(polyType: PolyType): MonoType =
  Instantiate().instantiate(polyType)

private case class Instantiate():
  private val mappings = scala.collection.mutable.HashMap.empty[String, String]

  def instantiate(polyType: PolyType): MonoType =
    polyType match
      case PolyType.Mono(MonoType.Concrete(c, args)) =>
        val args1 = args.map(arg => this.instantiate(PolyType.Mono(arg)))
        MonoType.Concrete(c, args1)
      case PolyType.Mono(MonoType.Var(v)) =>
        val v2 = mappings.getOrElse(v, v)
        MonoType.Var(v2)
      case PolyType.ForAll(v, p) =>
        mappings.update(v, freshIdent())
        this.instantiate(p)

def freeVars(monoType: MonoType): Set[String] =
  monoType match
    case MonoType.Var(v) => Set(v)
    case MonoType.Concrete(_, args) => args.flatMap(freeVars).toSet

def freeVars(polyType: PolyType): Set[String] =
  polyType match
    case PolyType.Mono(m) => freeVars(m)
    case PolyType.ForAll(v, m) => freeVars(m).filter(_ != v)

def freeVars(context: Context): Set[String] =
  context.values.flatMap(freeVars).toSet

def generalize(context: Context, polyType: PolyType): PolyType =
  val ctxFv = freeVars(context)
  val pFv = freeVars(polyType)
  val d = pFv diff ctxFv
  d.foldLeft(polyType)((p, v) => PolyType.ForAll(v, p))

case class TypeError(reason: String) extends Exception(reason)

def algorithmW(context: Context, expr: Expr): (Substitution, MonoType) =
  expr match
    case Expr.Var(ident) =>
        context.get(ident) match
          case None => throw TypeError(s"Unbound variable: `$ident`")
          case Some(p) => (Substitution.empty, instantiate(p))
    case Expr.Abs(param, body) =>
      val v = MonoType.Var(freshIdent())
      val (subst, ret) = algorithmW(
        context = context.updated(param, v),
        expr = body
      )
      val f = MonoType.concrete("->", v, ret)
      (subst, subst(f))
    case Expr.App(f, x) =>
      val fresh = MonoType.Var(freshIdent())
      val (s1, t1) = algorithmW(context, f)
      val (s2, t2) = algorithmW(s1(context), x)
      val s3 =
        unify(s2(t1), MonoType.concrete("->", t2, fresh)) match
          case Left(e) => throw TypeError(s"Unify error ($e)")
          case Right(s) => s
      (s3 compose s2 compose s1, s3(fresh))
    case Expr.Let(ident, value, body) =>
      val (s1, t1) = algorithmW(context, value)
      val (s2, t2) =
        val t1Gen = generalize(context, t1)
        val context2 = context.updated(ident, t1Gen)
        algorithmW(s1(context2), body)
      (s2 compose s1, t2)

enum TypeCheckError:
  case UnboundVar(name: String)
  case Unify(err: UnifyError)

def algorithmM(context: Context, expr: Expr, t: MonoType): Either[TypeCheckError, Substitution] =
  expr match
    case Expr.Var(ident) =>
      context.get(ident) match
        case None => Left(TypeCheckError.UnboundVar(ident))
        case Some(p) =>
          val m = instantiate(p)
          unify(t, m).left.map(TypeCheckError.Unify.apply)
    case Expr.Abs(param, body) =>
      val b1 = MonoType.Var(freshIdent())
      val b2 = MonoType.Var(freshIdent())
      for s1 <- unify(t, MonoType.concrete("->", b1, b2)).left.map(TypeCheckError.Unify.apply)
          newContext: Context = s1(context).updated(param, s1(b1))
          s2 <- algorithmM(newContext, body, s1(b2))
      yield s1 compose s2
    case Expr.App(f, x) =>
      val tParam = MonoType.Var(freshIdent())
      for s1 <- algorithmM(context, f, MonoType.concrete("->", tParam, t))
          newContext: Context = s1(context)
          s2 <- algorithmM(newContext, x, s1(tParam))
        yield s1 compose s2
    case Expr.Let(name, value, body) =>
      val tValue = MonoType.Var(freshIdent())
      for s1 <- algorithmM(context, value, tValue)
          newContext: Context = s1(context)
          generalizedTValue = generalize(newContext, s1(tValue))
          s2 <- algorithmM(newContext.updated(name, generalizedTValue), body, t)
        yield s1 compose s2
