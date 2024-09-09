package typechecker

import typechecker.UnifyError.{OccursCheck, TypeMismatch}
import typechecker.Type.{Named, Var}
import scala.collection.mutable

enum Type:
  case Var(ident: Int)
  case Named(name: String, args: List[Type] = Nil)

enum UnifyError:
  case OccursCheck
  case TypeMismatch


class Unifier {
  private val substitutions = new mutable.HashMap[Int, Type]()
  private var nextId = 0

  def freshVar(): Type = {
    val id = nextId
    this.nextId += 1
    Type.Var(id)
  }

  private def resolveOnce(t: Type): Type = t match
    case Named(_, _) => t
    case Var(id) => substitutions.getOrElse(id, t)

  def resolve(t: Type): Type = resolveOnce(t) match
    case Named(name, args) => Named(name, args.map(resolve))
    case t => t

  private def unifyArgs(args: List[(Type, Type)]): Either[UnifyError, Unit] = args match
    case Nil => Right(())
    case (t1, t2) :: tl =>
      for
        Unit <- unify(t1, t2)
        Unit <- unifyArgs(tl)
      yield Right(())

  private def occursIn(id: Int, t: Type): Boolean = resolveOnce(t) match
    case Var(id1) => id == id1
    case Named(_, args) => args.exists(t => occursIn(id, t))

  def unify(t1: Type, t2: Type): Either[UnifyError, Unit] =
    (resolveOnce(t1), resolveOnce(t2)) match
      case (Named(name1, _), Type.Named(name2, _)) if name1 != name2 => Left(TypeMismatch)
      case (Named(_, args1), Named(_, args2)) => unifyArgs(args1 zip args2)

      case (Var(id1), Var(id2)) if id1 == id2 => Right(())
      case (Var(id), t@Named(_, _)) if occursIn(id, t) => Left(OccursCheck)
      case (Var(id), _) =>
        substitutions.put(id, t2)
        Right(())
      case (Named(_, _), Var(_)) => unify(t2, t1)
}
