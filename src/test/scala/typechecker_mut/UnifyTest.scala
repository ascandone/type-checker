package typechecker_mut

import org.scalatest.funsuite.AnyFunSuite
import typechecker_mut.*
import typechecker_mut.Type.{Named, Record}

import scala.collection.immutable

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


  test("row types") {
    // https://ahnfelt.medium.com/row-polymorphism-crash-course-587f1e7b7c47
    val unifier = Unifier()
    val r0 = unifier.freshVar()
    val r1 = unifier.freshVar()


    // unify({ r0 | int: Int}, { r1 | str: String })
    //   r0 ~> { r2 | str: String }
    //   r1 ~> { r2 | int: Int }

    val r = Record(r0, immutable.Map(
      "int" -> Type.named("Int")
    ))
    val e = unifier.unify(
      r,
      Record(r1, immutable.Map(
        "str" -> Type.named("String")
      ))
    )
    assert(e == Right(()))

    assert(unifier.resolve(r) == Record(Type.Var(2), immutable.Map(
      "int" -> Type.named("Int"),
      "str" -> Type.named("String"),
    )))
  }

  test("fail to unify a row type with a named type") {
    val unifier = Unifier()
    val r0 = unifier.freshVar()

    val r = Record(unifier.freshVar(), immutable.Map())

    val e = unifier.unify(
      Record(unifier.freshVar(), immutable.Map()),
      Named("N")
    )
    assert(e == Left(UnifyError.TypeMismatch))
  }

  test("fail to unify row types with different fields") {
    val unifier = Unifier()

    val ra = Record(unifier.freshVar(), immutable.Map(
      "x" -> Named("Int")
    ))
    val rb = Record(unifier.freshVar(), immutable.Map(
      "x" -> Named("Bool")
    ))

    val e = unifier.unify(
      ra,
      rb
    )
    assert(e == Left(UnifyError.TypeMismatch))
  }


  test("unify identity error") {
    val unifier = Unifier()
    val t0 = unifier.freshVar()
    assert(unifier.unify(arrow(t0, t0), arrow(int, bool)) === Left(UnifyError.TypeMismatch))
  }
