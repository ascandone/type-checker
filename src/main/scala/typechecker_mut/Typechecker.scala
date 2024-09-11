package typechecker_mut

import lambda.{Declaration, Expr}

import scala.collection.immutable

type Context = immutable.Map[String, (TypeScheme, Type)]

enum TypecheckError:
  case UnifyError(t1: Type, t2: Type, unifyError: typechecker_mut.UnifyError)
  case UnboundVar(name: String)

class Typechecker(val unifier: Unifier = Unifier()) {

  private def unify(t1: Type, t2: Type) =
      unifier.unify(t1, t2).left.map(e => TypecheckError.UnifyError(t1, t2, e))


  def typecheckExpr(expr: Expr, t: Type, ctx: Context): Either[TypecheckError, Unit] = expr match
    case Expr.Var(name) => ctx.get(name) match
      case None => Left(TypecheckError.UnboundVar(name))
      case Some((scheme, typeLookup)) => unify(typeLookup, t)
    case Expr.App(f, x) =>
      val ft = unifier.freshVar()
      val xt = unifier.freshVar()
      for
        _ <- unify(Type.named("->", xt, t), ft)
        _ <- typecheckExpr(x, xt, ctx)
        _ <- typecheckExpr(f, ft, ctx)
      yield ()
    case Expr.Abs(param, body) =>
      val paramT = unifier.freshVar()
      val bodyT = unifier.freshVar()
      for
        _ <- unify(Type.named("->", paramT, bodyT), t)
        _ <- typecheckExpr(body, bodyT, ctx + (param -> (Set.empty, paramT)))
      yield ()
    case Expr.Let(name, value, body) =>
      val valueT = unifier.freshVar()
      for
        _ <- typecheckExpr(value, valueT, ctx)
        _ <- typecheckExpr(body, t, ctx + (name -> (Set.empty, valueT)))
      yield ()


  def typecheckDeclarations(declarations: List[Declaration], initialContext: Context = immutable.HashMap.empty): Either[TypecheckError, Context] =
    declarations.foldRight(Right(initialContext): Either[TypecheckError, Context])(
        (declaration, ctxResult) => declaration match
          case Declaration.Let(name, expr) =>
            val fresh = unifier.freshVar()
            for
              ctx <- ctxResult
              Unit <- typecheckExpr(expr, fresh, ctx)
            yield {
              val resolved = unifier.resolve(fresh)
              val scheme = generalise(resolved)
              ctx + (name -> (scheme, resolved))
            }
      )
}
