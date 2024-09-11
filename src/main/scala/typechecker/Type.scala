package typechecker

import scala.collection.{mutable, immutable}
import typechecker.UnifyError.{OccursCheck, TypeMismatch}
import typechecker.Type.{Named, Record, Var}
import scala.annotation.tailrec

enum Type:
  case Var(ident: Int)
  case Named(name: String, args: List[Type] = Nil)
  case Record(extra: Option[Type], fields: immutable.Map[String, Type])

object Type:
  def named(name: String, args: Type*): Type =
    Named(name, args.toList)

type TypeScheme = immutable.Set[Int]
def generalise(t: Type): TypeScheme = t match
  case Var(id) => immutable.Set(id)
  case Named(_, args) => args.map(generalise).foldLeft(Set.empty)(_ union _)
  case Record(cons, fields) =>
    val consScheme: TypeScheme = cons match
      case None => Set.empty
      case Some(cons) => generalise(cons)
    fields.values.map(generalise).foldLeft(consScheme)(_ union _)

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
    case Record(cons, fields) => Record(
      cons.map(loop),
      fields.map((k, v) => (k, loop(v)))
    )

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
    case t@Record(None, _) => t
    case t@Record(Some(r), fields) => resolve(r) match
      case Record(r2, fields2) => Record(r2, fields2 ++ fields)
      case t@Var(_) => Record(Some(t), fields)
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
    case Record(extra, fields) => fields.values.exists(t => occursIn(id, t)) || (extra match {
      case None => false
      case Some(extra) => occursIn(id, extra)
    })

  def unify(t1: Type, t2: Type): Either[UnifyError, Unit] =
    (resolveOnce(t1), resolveOnce(t2)) match
      case (Named(_, _), Record(_, _)) => Left(TypeMismatch)
      case (Record(_, _), Named(_, _)) => Left(TypeMismatch)

      case (Named(name1, _), Named(name2, _)) if name1 != name2 => Left(TypeMismatch)
      case (Named(_, args1), Named(_, args2)) => unifyArgs(args1 zip args2)

      // records must be exactly alike when they are closed
      case (Record(None, fields1), Record(None, fields2)) =>

        val eqFields = fields1.toList.forall((field, _) => fields2.contains(field)) &&
                fields2.toList.forall((field, _) => fields1.contains(field))

        if !eqFields then {
          return Left(TypeMismatch)
        }

        unify(
          Record(Some(freshVar()), fields1),
          Record(Some(freshVar()), fields2),
        )

      case (Record(r1o, fields1), Record(r2o, fields2)) =>
        // TODO check fields1 and fields2 are compatible?
        val r3 = freshVar()
        for
          _ <- unifyArgs(
            // this could be optimized
            for
              (name1, t1) <- fields1.toList
              (name2, t2) <- fields2.toList
              if name1 == name2
            yield (t1, t2)
          )
          _ <- r1o match
            case Some(r1) => unify(r1, Record(Some(r3), fields2))
            case None => Right(())
          _ <- r2o match
            case Some(r2) => unify(r2, Record(Some(r3), fields1))
            case None => Right(())
        yield ()

      case (Var(id1), Var(id2)) if id1 == id2 => Right(())
      case (Var(id), t) if occursIn(id, t) => Left(OccursCheck)

      case (Var(id), _) =>
        substitutions.put(id, t2)
        Right(())

      case (_, Var(_)) => unify(t2, t1)
}
