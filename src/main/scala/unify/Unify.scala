package unify

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, Map}

// Ident for type variables
type Ident = String

enum MonoType:
  case Var(ident: Ident)
  case Concrete(name: String, args: List[MonoType] = Nil)

object MonoType:
  def concrete(name: String, args: MonoType*): MonoType =
    MonoType.Concrete(name, args.toList)

type Substitution = Map[Ident, MonoType]

def applySubstitution(substitution: Substitution, monoType: MonoType): MonoType =
  monoType match
    case MonoType.Var(ident) =>
      substitution.getOrElse(ident, monoType)
    case MonoType.Concrete(name, args) =>
      val mappedArgs = args.map(applySubstitution(substitution, _))
      MonoType.Concrete(name, mappedArgs)

/// Return a substitution that behaves in the same way as applying s1 and then s2
def composeSubstitution(s1: Substitution, s2: Substitution): Substitution =
  val entries = (s1.keys ++ s2.keys).map(k =>
    val m = applySubstitution(
      substitution = s2,
      monoType = applySubstitution(
        substitution = s1,
        monoType = MonoType.Var(k)
      )
    )

    (k, m)
  )
  HashMap.from(entries)

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
      Right(HashMap.empty)

    case (MonoType.Var(v1), _) if occursCheck(v1, t2) =>
      Left(UnifyError.OccursCheck)

    case (MonoType.Var(v1), _) =>
      Right(HashMap(v1 -> t2))

    case (_, MonoType.Var(v2)) =>
      unify(t2, t1)

private def occursCheck(v: Ident, t: MonoType): Boolean =
  t match
    case MonoType.Var(v2) => v == v2
    case MonoType.Concrete(_, args) =>
      args.exists(occursCheck(v, _))

private def unifyTraverse(args1: List[MonoType], args2: List[MonoType], acc: Substitution = HashMap.empty): Either[UnifyError, Substitution] =
  (args1, args2) match
    case (Nil, Nil) => Right(HashMap.empty)
    case (_ :: _, Nil) | (Nil, _ :: _) => Left(UnifyError.CannotUnify)
    case (t1 :: tl1, t2 :: tl2) =>
      val t1_ = applySubstitution(acc, t1)
      val t2_ = applySubstitution(acc, t2)
      for sub <- unify(t1_, t2_)
          sub2 <- unifyTraverse(tl1, tl2, sub)
      yield composeSubstitution(sub2, sub)

enum PolyType:
  case Mono(t: MonoType)
  case ForAll(name: Ident, p: PolyType)

def applySubstitution(substitution: Substitution, polyType: PolyType): PolyType =
  polyType match
    case PolyType.Mono(m) => PolyType.Mono(applySubstitution(substitution, m))
    case PolyType.ForAll(v, m) =>
      // TODO why not substitution.removed(v) ?
      val m1 = applySubstitution(substitution, m)
      PolyType.ForAll(v, m1)

def freshVar(): String =
  java.util.UUID.randomUUID().toString

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
        mappings.update(v, freshVar())
        this.instantiate(p)

