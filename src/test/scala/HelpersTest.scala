import org.scalatest.funsuite.AnyFunSuite
import unify.*

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
