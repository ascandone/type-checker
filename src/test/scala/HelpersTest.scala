import org.scalatest.funsuite.AnyFunSuite
import unify.monoToPoly
import unify.*

import scala.collection.immutable.HashMap

class HelpersTest extends AnyFunSuite:
  test("instantiation of concrete types should preserve the type") {
    val mt = MonoType.Concrete("Int")

    assert(instantiate(PolyType.Mono(mt)) === mt)
  }

  test("instantiation of monotypes should preserve the var when not mapped") {
    val mt = MonoType.Var("a")
    assert(instantiate(PolyType.Mono(mt)) === mt)
  }

  test("instantiation of quantified vars") {
    val mt = MonoType.concrete("Pair", MonoType.Var("a"), MonoType.Var("a"))
    val pt = PolyType.ForAll("a", PolyType.Mono(mt))

    val out = instantiate(pt)
    out match {
      case MonoType.Concrete("Pair", MonoType.Var(v1) :: MonoType.Var(v2) :: Nil) =>
          assert(v1 === v2)
          assert(v1 !== "a")

      case _ => fail(out.toString)
    }
  }

  test("all vars should be free in monotype") {
    assert(
      freeVars(PolyType.Mono(MonoType.Var("a"))) === Set("a")
    )

    val m = MonoType.concrete("Pair",
      MonoType.Var("a"),
      MonoType.Var("b"))

    assert(
      freeVars(PolyType.Mono(m)) === Set("a", "b")
    )
  }

  test("quantified vars should not be free") {
    val m = MonoType.concrete("Pair",
      MonoType.Var("a"),
      MonoType.Var("b"))

    val p = PolyType.ForAll("a", PolyType.Mono(m))

    assert(freeVars(p) === Set("b"))
  }

  test("generalize") {
    val ctx: Context = HashMap(
      "x" -> MonoType.Var("b"),
      "y" -> MonoType.concrete("->",
          MonoType.concrete("List", MonoType.Var("c")),
        MonoType.concrete("Int"),
      ),
      "z" -> PolyType.ForAll("d", MonoType.Var("d"))
    )

    val m = MonoType.concrete("->",
      MonoType.Var("a"),
      MonoType.Var("b"),
      MonoType.Var("c"),
      MonoType.Var("d"),
      MonoType.Var("e"))

    val p = PolyType.ForAll("e", m)

    val result = generalize(ctx, p)
    result match {
      case PolyType.ForAll(a,
          PolyType.ForAll(b,
            PolyType.ForAll(c, PolyType.Mono(m1)))) =>
              assert(Set(a, b, c) === Set("a", "d", "e"))
              assert(m == m1)

      case _ => fail(result.toString)
    }
  }

