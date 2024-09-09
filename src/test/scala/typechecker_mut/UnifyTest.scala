package typechecker_mut

import org.scalatest.funsuite.AnyFunSuite
import typechecker_mut.*

def int = Type.Named("Int")
def bool = Type.Named("Bool")
def list(t1: Type) = Type.named("List", t1)
def arrow(t1: Type, t2: Type) = Type.named("->", t1, t2)

class UnifyTest extends AnyFunSuite:
  test("unifying two different concrete types should fail") {
    val unifier = Unifier()

    assert(unifier.unify(bool, int) === Left(UnifyError.TypeMismatch))
  }

  test("unifying two concrete types equal to each other should return an empty subst") {
    val unifier = Unifier()

    assert(unifier.unify(int, int) === Right(()))
  }

  test("unifying two concrete types with different args should fail") {
    val unifier = Unifier()

    assert(unifier.unify(list(bool), list(int)) === Left(UnifyError.TypeMismatch))
  }

  test("unifying two equal variables should return the empty subst") {
    val unifier = Unifier()
    val t0 = unifier.freshVar()
    val t1 = unifier.freshVar()

    assert(unifier.unify(t0, t1) === Right(()))
  }

  test("occurs check failure should not unify") {
    val unifier = Unifier()
    val t0 = unifier.freshVar()

    assert(unifier.unify(t0, list(t0)) === Left(UnifyError.OccursCheck))
  }

  test("a variable should be unified with a concrete type") {
    val unifier = Unifier()
    val t0 = unifier.freshVar()

    assert(unifier.unify(t0, bool) === Right(()))
    assert(unifier.resolve(t0) == bool)
  }

  test("a variable should be unified with a concrete type, inverse order") {
    val unifier = Unifier()
    val t0 = unifier.freshVar()

    assert(unifier.unify(bool, t0) === Right(()))
    assert(unifier.resolve(t0) == bool)
  }

  test("two different vars should unify") {
    val unifier = Unifier()
    val t0 = unifier.freshVar()
    val t1 = unifier.freshVar()

    assert(unifier.unify(t0, t1) === Right(()))
    assert(unifier.resolve(t0) == unifier.resolve(t1))
  }

  test("two different nested vars should unify") {
    val unifier = Unifier()
    val t0 = unifier.freshVar()
    val t1 = unifier.freshVar()

    assert(unifier.unify(list(t0), list(t1)) === Right(()))
    assert(unifier.resolve(t0) == unifier.resolve(t1))
  }

  test("unify invariant") {
    val unifier = Unifier()
    val t0 = unifier.freshVar()
    val t1 = unifier.freshVar()

    assert(unifier.unify(arrow(t0, bool), arrow(int, t1)) == Right(()))
    assert(unifier.resolve(t0) == int)
    assert(unifier.resolve(t1) == bool)
  }

  test("composing transitive unification") {
    val unifier = Unifier()
    val t0 = unifier.freshVar()
    val t1 = unifier.freshVar()

    unifier.unify(t0, t1)
    unifier.unify(t0, int)

    assert(unifier.resolve(t0) == int)
    // assert(unifier.resolve(t2) == int)
    // TODO swapped
  }


  test("unify identity error") {
    val unifier = Unifier()
    val t0 = unifier.freshVar()
    assert(unifier.unify(arrow(t0, t0), arrow(int, bool)) === Left(UnifyError.TypeMismatch))
  }
