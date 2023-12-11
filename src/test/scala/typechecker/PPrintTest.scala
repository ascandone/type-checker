package typechecker

import org.scalatest.funsuite.AnyFunSuite

class PPrintTest extends AnyFunSuite:
  test("var") {
    assert(
      pprint(MonoType.Var("x")) == "x"
    )
  }

  test("concrete without args") {
    assert(
      pprint(MonoType.concrete("Int")) == "Int"
    )
  }

  test("concrete with args") {
    assert(
      pprint(MonoType.concrete("Either", MonoType.Var("a"), MonoType.Var("b"))) == "Either a b"
    )
  }

  test("nested arg") {
    val m = MonoType.concrete("Either",
        MonoType.concrete("List", MonoType.Var("a")),
        MonoType.Var("b"))

    assert(
      pprint(m) == "Either (List a) b"
    )
  }

  test("fn type should be infix") {
    val m = MonoType.concrete("->",
      MonoType.Var("a"),
      MonoType.Var("b"))

    assert(
      pprint(m) == "a -> b"
    )
  }

  test("nested fn without parens should be right assoc") {
    val m = MonoType.concrete("->",
      MonoType.Var("a"),
      MonoType.concrete("->",
        MonoType.Var("b"),
        MonoType.Var("c")))

    assert(
      pprint(m) == "a -> b -> c"
    )
  }

  test("nested fn with parens") {
    val m = MonoType.concrete("->",
      MonoType.concrete("->",
        MonoType.Var("a"),
        MonoType.Var("b")),
      MonoType.Var("c"))

    assert(
      pprint(m) == "(a -> b) -> c"
    )
  }

  test("polytype should normalize vars") {
    val m =
        PolyType.ForAll("t1",
          PolyType.ForAll("t0",
            MonoType.concrete("T",
              MonoType.Var("t0"),
              MonoType.Var("t0"),
              MonoType.Var("t1"))))

    assert(
      pprint(m) == "T a a b"
    )
  }


