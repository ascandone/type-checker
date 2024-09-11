package typechecker

import scala.collection.{mutable, immutable}
import typechecker.UnifyError.{OccursCheck, TypeMismatch}
import typechecker.Type.{Named, Record, Var}
import scala.annotation.tailrec

enum Type:
  case Var(ident: Int)
  case Named(name: String, args: List[Type] = Nil)
  case Record(extra: Type, fields: immutable.Map[String, Type])

object Type:
  def named(name: String, args: Type*): Type =
    Named(name, args.toList)

type TypeScheme = immutable.Set[Int]
def generalise(t: Type): TypeScheme = t match
  case Var(id) => immutable.Set(id)
  case Named(_, args) => args.map(generalise).foldLeft(Set.empty)(_ union _)

def instantiate(unifier: Unifier, scheme: TypeScheme, t: Type): Type = {
  val instantiated = mutable.HashMap[Int, Type]()

  def loop(t: Type): Type = t match
    case Var(id) if scheme.contains(id) => instantiated.get(id) match
      case Some(t) => t
      case None =>
        val t = unifier.freshVar()
        instantiated.put(id, t)
        t
    case Var(_) => t
    case Named(name, args) => Named(name, args.map(loop))

  loop(t)
}


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

  @tailrec
  private def resolveOnce(t: Type): Type = t match
    case Named(_, _) | Record(_, _) => t
    case Var(id) => substitutions.get(id) match
      case Some(t) => resolveOnce(t)
      case None => t

  def resolve(t: Type): Type = resolveOnce(t) match
    case Named(name, args) => Named(name, args.map(resolve))
    case t@Record(r, fields) => resolve(r) match
      case Record(r2, fields2) => Record(r2, fields2 ++ fields)
      case t@Var(_) => Record(t, fields)
      case _ => t
    case t@Var(_) => t

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
    case Record(extra, fields) => fields.values.exists(t => occursIn(id, t)) || occursIn(id, extra)

  def unify(t1: Type, t2: Type): Either[UnifyError, Unit] =
    (resolveOnce(t1), resolveOnce(t2)) match
      case (Named(_, _), Record(_, _)) => Left(TypeMismatch)
      case (Record(_, _), Named(_, _)) => Left(TypeMismatch)

      case (Named(name1, _), Named(name2, _)) if name1 != name2 => Left(TypeMismatch)
      case (Named(_, args1), Named(_, args2)) => unifyArgs(args1 zip args2)

      case (Record(r1, fields1), Record(r2, fields2)) =>
        val a1 = fields1.toList.flatMap((k, t1) => fields2.get(k) match
          case None => None
          case Some(t2) => Some((t1, t2))
        )

        val a2 = fields1.toList.flatMap((k, t2) => fields2.get(k) match
          case None => None
          case Some(t2) => Some((t1, t2))
        )

        // TODO check fields1 and fields2 are compatible?
        val r3 = freshVar()
        for
          _ <- unifyArgs(a1)
          _ <- unifyArgs(a2)
          _ <- unify(r1, Record(r3, fields2))
          _ <- unify(r2, Record(r3, fields1))
        yield ()

      case (Var(id1), Var(id2)) if id1 == id2 => Right(())
      case (Var(id), t) if occursIn(id, t) => Left(OccursCheck)

      case (Var(id), _) =>
        substitutions.put(id, t2)
        Right(())

      case (_, Var(_)) => unify(t2, t1)
}
