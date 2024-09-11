package typechecker

import org.scalatest.funsuite.AnyFunSuite
import typechecker.*
import typechecker.Type.{Named, Record}
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

  test("generalization") {
    val t = Type.Named("Tuple2", List(
      Type.Var(0),
      Type.Var(1),
      Type.Var(0),
    ))

    assert(generalise(t) == Set(0, 1))
  }

  test("instantiation") {
    val unifier = Unifier()

    val t0 = unifier.freshVar()
    val t1 = unifier.freshVar()

    val t = Type.Named("Tuple2", List(
      t0,
      t1,
      t0,
    ))

    val scheme = generalise(t)

    assert(instantiate(unifier, scheme, t) == Type.Named("Tuple2", List(
      Type.Var(2),
      Type.Var(3),
      Type.Var(2),
    )))
  }

  test("unify identity error") {
    val unifier = Unifier()
    val t0 = unifier.freshVar()
    assert(unifier.unify(arrow(t0, t0), arrow(int, bool)) === Left(UnifyError.TypeMismatch))
  }

  test("row types") {
    // https://ahnfelt.medium.com/row-polymorphism-crash-course-587f1e7b7c47
    val unifier = Unifier()
    val r0 = unifier.freshVar()
    val r1 = unifier.freshVar()


    // unify({ r0 | int: Int}, { r1 | str: String })
    //   r0 ~> { r2 | str: String }
    //   r1 ~> { r2 | int: Int }

    val r = Record(Some(r0), immutable.Map(
      "int" -> Type.named("Int")
    ))
    val e = unifier.unify(
      r,
      Record(Some(r1), immutable.Map(
        "str" -> Type.named("String")
      ))
    )
    assert(e == Right(()))

    assert(unifier.resolve(r) == Record(Some(Type.Var(2)), immutable.Map(
      "int" -> Type.named("Int"),
      "str" -> Type.named("String"),
    )))
  }

  test("fail to unify a row type with a named type") {
    val unifier = Unifier()
    val r0 = unifier.freshVar()

    val r = Record(Some(unifier.freshVar()), immutable.Map())

    val e = unifier.unify(
      Record(Some(unifier.freshVar()), immutable.Map()),
      Named("N")
    )
    assert(e == Left(UnifyError.TypeMismatch))
  }

  test("fail to unify row types with different fields") {
    val unifier = Unifier()

    val ra = Record(Some(unifier.freshVar()), immutable.Map(
      "x" -> Named("Int")
    ))
    val rb = Record(Some(unifier.freshVar()), immutable.Map(
      "x" -> Named("Bool")
    ))

    val e = unifier.unify(
      ra,
      rb
    )
    assert(e == Left(UnifyError.TypeMismatch))
  }

  test("fail to unify closed records with different fields") {
    val unifier = Unifier()

    val ra = Record(None, immutable.Map(
      "x" -> Named("Int")
    ))
    val rb = Record(None, immutable.Map(
      "y" -> Named("Bool")
    ))

    val e = unifier.unify(
      ra,
      rb
    )
    println(unifier.resolve(ra))
    println(unifier.resolve(rb))
    assert(e == Left(UnifyError.TypeMismatch))
  }

  test("unify closed records with same fields") {
    val unifier = Unifier()

    val ra = Record(None, immutable.Map(
      "x" -> Named("Int")
    ))
    val rb = Record(None, immutable.Map(
      "x" -> Named("Int")
    ))

    val e = unifier.unify(
      ra,
      rb
    )
    assert(e == Right(()))
  }

  test("unify open record with a closed record") {
    val unifier = Unifier()

    val t0 = unifier.freshVar()
    val ra = Record(Some(t0), immutable.Map(
      "y" -> Named("Unit")
    ))
    val rb = Record(None, immutable.Map(
      "x" -> Named("Unit")
    ))

    val e = unifier.unify(
      ra,
      rb
    )
    assert(e == Right(()))
    assert(unifier.resolve(t0) == Record(
      Some(Type.Var(1)),
      Map(
        "x" -> Named("Unit"),
      )
    ))
  }