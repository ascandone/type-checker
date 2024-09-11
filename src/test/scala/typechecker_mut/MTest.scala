package typechecker_mut

import lambda.{Declaration, Expr, Parser}
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.HashMap

class MTest extends AnyFunSuite:
  test("type check mono variable bound in context") {
    val ctx: Context = HashMap(
      "one" -> (Set.empty, Type.named("Int"))
    )

    val t = typesOf("let x = one", ctx)

    assert(t == HashMap(
      "one" -> "Int",
      "x" -> "Int",
    ))
  }

  test("type check abstraction") {
    val t = typesOf("let f = \\x -> x")

    assert(t == HashMap(
      "f" -> "t0 -> t0"
    ))
  }

  test("type check abstraction using a fn") {
    val ctx: Context = HashMap("iseven" -> (Set.empty, Type.named("->", Type.named("Int"), Type.named("Bool"))))
    val t = typesOf("let f = \\x -> iseven x", ctx)

    assert(t == HashMap(
      "iseven" -> "Int -> Bool",
      "f" -> "Int -> Bool"
    ))
  }

  test("type check complex abstr") {
    val t = typesOf("let f = \\x -> \\y -> y x")

    assert(t == HashMap(
      "f" -> "t0 -> (t0 -> t1) -> t1"
    ))
  }


  def typesOf(src: String, context: Context = HashMap.empty): Map[String, String] = {
    val unifier = Unifier()
    val tc = Typechecker(unifier)

    val parsed = Parser(src).get
    val ctx = tc.typecheckDeclarations(parsed, context).toOption.get
    ctx.map((k, v) => (k, pprint(v._2)))
  }
