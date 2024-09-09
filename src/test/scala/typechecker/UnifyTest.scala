package typechecker

import org.scalatest.funsuite.AnyFunSuite
import typechecker.*
import typechecker.MonoType.Concrete

class UnifyTest extends AnyFunSuite:
  test("unifying two different concrete types should fail") {
    val t1 = MonoType.concrete("Bool")
    val t2 = MonoType.concrete("Int")

    assert(unify(t1, t2) === Left(UnifyError.CannotUnify))
  }

  test("unifying two concrete types equal to each other should return an empty subst") {
    val t1 = MonoType.concrete("Int")
    val t2 = MonoType.concrete("Int")

    assert(unify(t1, t2) === Right(Substitution.empty))
  }

  test("unifying two concrete types with different args should fail") {
    val t1 = MonoType.concrete("List", MonoType.concrete("Bool"))
    val t2 = MonoType.concrete("List", MonoType.concrete("Int"))

    assert(unify(t1, t2) === Left(UnifyError.CannotUnify))
  }

  test("unifying two equal variables should return the empty subst") {
    val t1 = MonoType.Var("a")
    val t2 = MonoType.Var("a")

    assert(unify(t1, t2) === Right(Substitution.empty))
  }

  test("occurs check failure should not unify") {
    val t1 = MonoType.Var("a")
    val t2 = MonoType.concrete("List", MonoType.Var("a"))

    assert(unify(t1, t2) === Left(UnifyError.OccursCheck))
  }

  test("a variable should be unified with a concrete type") {
    val t1 = MonoType.Var("a")
    val t2 = MonoType.concrete("Bool")

    assert(unify(t1, t2) === Right(Substitution.fromEntries(
      "a" -> MonoType.concrete("Bool")
    )))
  }

  test("a variable should be unified with a concrete type, inverse order") {
    val t1 = MonoType.concrete("Bool")
    val t2 = MonoType.Var("a")

    assert(unify(t1, t2) === Right(Substitution.fromEntries(
      "a" -> MonoType.concrete("Bool")
    )))
  }

  test("two different vars should unify") {
    val t1 = MonoType.Var("a")
    val t2 = MonoType.Var("b")

    assert(unify(t1, t2) === Right(Substitution.fromEntries(
      "a" -> MonoType.Var("b")
    )))
  }

  test("two different nested vars should unify") {
    val t1 = MonoType.concrete("List", MonoType.Var("a"))
    val t2 = MonoType.concrete("List", MonoType.Var("b"))

    assert(unify(t1, t2) === Right(Substitution.fromEntries(
      "a" -> MonoType.Var("b")
    )))
  }

  test("unify error on second arg") {
    val t1 = MonoType.concrete("->", MonoType.Var("a"), MonoType.Concrete("Int"))
    val t2 = MonoType.concrete("->", MonoType.Var("a"), MonoType.Concrete("Bool"))

    assert(unify(t1, t2) === Left(UnifyError.CannotUnify))
  }

  test("merge unify constraints") {
    val t1 = MonoType.concrete("->", MonoType.Var("a1"), MonoType.Var("b1"))
    val t2 = MonoType.concrete("->", MonoType.Var("a2"), MonoType.Var("b2"))

    assert(unify(t1, t2) === Right(Substitution.fromEntries(
      "a1" -> MonoType.Var("a2"),
      "b1" -> MonoType.Var("b2"),
    )))
  }

  test("unify invariant") {
    val t1 = MonoType.concrete("->",
        MonoType.Var("a"),
        MonoType.concrete("Bool"))

    val t2 = MonoType.concrete("->",
        MonoType.concrete("Int"),
        MonoType.Var("b"),
      )

    val subst = unify(t1, t2).getOrElse {
      throw new Error("Invalid subst")
    }

    assert(subst(t1) == subst(t2))
  }

  test("composing transitive unification") {
    val a = MonoType.Var("a")
    val b = MonoType.Var("b")
    val int = MonoType.concrete("Int")

    val s =
      // s1 = {a => b}
      // s1(a) = b

      for s1 <- unify(a, b)
        s2 <- unify(s1(a), int)
      yield  s1 compose s2

    assert(s === Right(Substitution.fromEntries(
      "a" -> MonoType.concrete("Int"),
      "b" -> MonoType.concrete("Int"),
    )))
  }


  test("unify identity error") {
    val t1 =
      MonoType.concrete("->",
        MonoType.Var("a"),
        MonoType.Var("a")
      )

    val t2 =
      MonoType.concrete("->",
        MonoType.concrete("Bool"),
        MonoType.concrete("Int")
      )

    assert(unify(t1, t2) === Left(UnifyError.CannotUnify))
  }
