package typechecker

import org.scalatest.funsuite.AnyFunSuite
import typechecker.Type.Var

class PPrintTest extends AnyFunSuite:
  test("var") {
    assert(
      pprint(Var(0)) == "t0"
    )
  }

  test("concrete without args") {
    assert(
      pprint(Type.named("Int")) == "Int"
    )
  }

  test("concrete with args") {
    val t = Type.named("Either", Var(0), Var(1))
    assert(
      pprint(t) == "Either t0 t1"
    )
  }

  test("nested arg") {
    val m = Type.named("Either",
      Type.named("List", Var(0)),
      Var(1))

    assert(
      pprint(m) == "Either (List t0) t1"
    )
  }

  test("fn type should be infix") {
    val m = Type.named("->",
      Var(0),
      Var(1))

    assert(
      pprint(m) == "t0 -> t1"
    )
  }

  test("nested fn without parens should be right assoc") {
    val m = Type.named("->",
      Var(0),
      Type.named("->",
        Var(1),
        Var(2)))

    assert(
      pprint(m) == "t0 -> t1 -> t2"
    )
  }

  test("nested fn with parens") {
    val m = Type.named("->",
      Type.named("->",
        Var(0),
        Var(1)),
      Var(2))

    assert(
      pprint(m) == "(t0 -> t1) -> t2"
    )
  }
