import org.scalatest.funsuite.AnyFunSuite
import typechecker.*
import typechecker.monoToPoly

import scala.collection.immutable.HashMap

class WTest extends AnyFunSuite:
  test("type check mono variable bound in context") {
    val e = Expr.Var("one")
    val ctx: Context = HashMap(
      "one" -> MonoType.concrete("Int")
    )


    val result = (Substitution.empty, MonoType.concrete("Int"))
    assert(algorithmW(ctx, e) == result)
  }

  test("type check poly variable bound in context") {
    val e = Expr.Var("id")
    val ctx: Context = HashMap(
      "id" -> PolyType.ForAll("a", MonoType.Var("a"))
    )

    val out = algorithmW(ctx, e)
    out match
      case (subst, MonoType.Var(v)) =>
        assert(subst == Substitution.empty)
        assert(v != "a")
      case _ => fail(out.toString())
  }

  test("should throw when var is not in ctx ") {
    val e = Expr.Var("one")

    assertThrows[TypeError] {
      algorithmW(HashMap.empty, e)
    }
  }

  test("type check abstraction") {
    val e = Expr.Abs("a", Expr.Var("a"))
    val result = (HashMap.empty, MonoType.concrete("Int"))
    val out = algorithmW(HashMap.empty, e)

    out match
      case (subst, MonoType.Concrete("->", MonoType.Var(v1) :: MonoType.Var(v2) :: Nil)) =>
        assert(subst == Substitution.empty)
        assert(v1 == v2)
        assert(v1 != "a")
      case _ => fail(out.toString())
  }
