import org.scalatest.funsuite.AnyFunSuite
import typechecker.*

import scala.collection.immutable.HashMap;

class SubstitutionTest extends AnyFunSuite:
  test("Substitution should not change types without vars") {
    val s = Substitution.fromEntries(
      "a" -> MonoType.Var("b"),
    )

    val t = MonoType.concrete("List", MonoType.concrete("Bool"))

    assert(s(t) === t)
  }

  test("Monotypes should substitute vars") {
    val s = Substitution.fromEntries(
      "a" -> MonoType.concrete("Bool"),
    )

    val t = MonoType.concrete("Apply", MonoType.Var("a"), MonoType.Var("b"))

    val result = MonoType.concrete("Apply", MonoType.concrete("Bool"), MonoType.Var("b"))

    assert(s(t) === result)
  }

  test("Mutually exclusive substs") {
    val s1 = Substitution.fromEntries(
      "a" -> MonoType.concrete("X"),
    )

    val s2 = Substitution.fromEntries(
      "b" -> MonoType.concrete("Y"),
    )

    val result = Substitution.fromEntries(
      "a" -> MonoType.concrete("X"),
      "b" -> MonoType.concrete("Y"),
    )

    assert(s1.compose(s2) === result)
  }

  test("Substs on same sym") {
    val s1 = Substitution.fromEntries(
      "a" -> MonoType.concrete("X"),
    )

    val s2 = Substitution.fromEntries(
      "a" -> MonoType.concrete("Y"),
    )

    val result = Substitution.fromEntries(
      "a" -> MonoType.concrete("X"),
    )

    assert(s1.compose(s2) === result)
  }

  test("Concatenated substs") {
    val s1 = Substitution.fromEntries(
      "a" -> MonoType.Var("b"),
    )

    val s2 = Substitution.fromEntries(
      "b" -> MonoType.concrete("X"),
    )

    val result = Substitution.fromEntries(
      "a" -> MonoType.concrete("X"),
      "b" -> MonoType.concrete("X"),
    )

    assert(s1.compose(s2) === result)
  }

  test("poly subst should substitute quantified vars") {
    val s = Substitution.fromEntries(
      "a" -> MonoType.concrete("Int"),
    )

    val m = MonoType.Var("a")
    val t = PolyType.ForAll("a", PolyType.Mono(m))

    val result = PolyType.ForAll("a", PolyType.Mono(MonoType.concrete("Int")))
    assert(s(t) === result)
  }

  test("poly subst should substitute free vars") {
    val s = Substitution.fromEntries(
      "a" -> MonoType.concrete("Int"),
    )

    val m = MonoType.Var("a")
    val t = PolyType.ForAll("x", PolyType.Mono(m))

    val result = PolyType.ForAll("x", PolyType.Mono(MonoType.concrete("Int")))
    assert(s(t) === result)
  }
