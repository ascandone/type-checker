package typechecker

import lambda.{Declaration, Expr, Parser}
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.HashMap

class MTest extends AnyFunSuite:
  test("type check mono variable bound in context") {

    val ctx: Context = HashMap(
      "one" -> MonoType.concrete("Int")
    )

    val t = typesOf("let x = one", ctx)

    assert(t == HashMap(
      "x" -> "Int"
    ))
  }

  test("type check abstraction") {
    val t = typesOf("let f = \\x -> x")

    assert(t == HashMap(
      "f" -> "a -> a"
    ))
  }

  test("type check abstraction using a fn") {
    val ctx: Context = HashMap("iseven" -> MonoType.concrete("->", MonoType.concrete("Int"), MonoType.concrete("Bool")))
    val t = typesOf("let f = \\x -> iseven x", ctx)

    assert(t == HashMap(
      "f" -> "Int -> Bool"
    ))
  }

  test("type check complex abstr") {
    val t = typesOf("let f = \\x -> \\y -> y x")

    assert(t == HashMap(
      "f" -> "a -> (a -> b) -> b"
    ))
  }


  def typesOf(src: String, context: Context = HashMap.empty) = {
    val parsed = Parser(src).get
    parsed.foldRight(HashMap.empty)((decl, map: HashMap[String, String]) => {
      decl match {
        case Declaration.Let(name, expr) =>
          val m = MonoType.Var(freshIdent())
          val s = algorithmM(context, expr, m).toOption.get
          val res = generalize(context, s(m))
          map.updated(name, pprint(res))
      }
    })

  }
