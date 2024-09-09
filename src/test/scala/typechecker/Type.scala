package typechecker

import typechecker.UnifyError.{OccursCheck, TypeMismatch}

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

  private def resolveOnce(t: Type): Type =  t match {
    case Type.Named(_, _) => t
    case Type.Var(id) => substitutions.getOrElse(id, t)
  }

  // TODO resolve inner

  private def unifyArgs(args: List[(Type, Type)]): Either[UnifyError, Unit] = args match {
    case Nil => Right(())
    case (t1, t2) :: tl =>
      for
        Unit <- unify(t1, t2)
        Unit <- unifyArgs(tl)
      yield Right(())
  }

  // TODO impl
  private def occursIn(id: Int, t: Type) = false

  def unify(t1: Type, t2: Type): Either[UnifyError, Unit] =
    (resolveOnce(t1), resolveOnce(t2)) match {
      case (Type.Named(name1, _), Type.Named(name2, _)) if name1 != name2 => Left(TypeMismatch)
      case (Type.Named(_, args1), Type.Named(_, args2)) => unifyArgs(args1 zip args2)

      case (Type.Var(id1), Type.Var(id2)) if id1 == id2 => Right(())
      case (Type.Var(id), t@Type.Named(_, _)) if occursIn(id, t) => Left(OccursCheck)
      case (Type.Var(id), _) =>
        substitutions.put(id, t2)
        Right(())
      case (Type.Named(_, _), Type.Var(_)) => unify(t2, t1)
    }
}


