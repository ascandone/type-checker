package typechecker

import lambda.Parser
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
      "f" -> "Int -> Bool"
    ))
  }

  test("type check complex abstr") {
    val t = typesOf("let f = \\x -> \\y -> y x")

    assert(t == HashMap(
      "f" -> "t0 -> (t0 -> t1) -> t1"
    ))
  }


  test("lookup previously defined values") {
    val t = typesOf(
      """
        | let id = \a -> a
        | let y = id
        |""".stripMargin)

    assert(t == HashMap(
      "id" -> "t0 -> t0",
      "y" -> "t0 -> t0",
    ))
  }

  test("generalise top-level let") {
    val ctx: Context = HashMap(
      "unit" -> (Set.empty, Type.named("Unit")),
      "one" -> (Set.empty, Type.named("Num"))
    )
    val t = typesOf(
      """
        | let id = \a -> a
        | let x = id unit
        | let y = id one
        |""".stripMargin,
      ctx)

    assert(t == HashMap(
      "id" -> "t0 -> t0",
      "x" -> "Unit",
      "y" -> "Num",
    ))
  }

  test("create poly variant") {
    val ctx: Context = HashMap(
      "unit" -> (Set.empty, Type.named("Unit"))
    )

    // val t = typesOf("let x = Poly unit")

    // assert(t == HashMap("x" -> "[t0 | Poly Unit]"))
  }


  def typesOf(src: String, context: Context = HashMap.empty): Map[String, String] = {
    val unifier = Unifier()
    val tc = Typechecker(unifier)

    val parsed = Parser(src).get
    val ctx = tc.typecheckDeclarations(parsed, context) match {
      case Left(e) => {
        throw new Error(e.toString)
      }
      case Right(x) => x
    }
    (ctx -- context.keys).map((k, v) => (
      k,
      pprint(v._2)
    ))
  }
