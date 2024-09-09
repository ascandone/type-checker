package typechecker_mut

import lambda.{Declaration, Expr, Parser}
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.HashMap

class MTest extends AnyFunSuite:
  test("type check mono variable bound in context") {

    val ctx: Context = HashMap(
      "one" -> Type.named("Int")
    )

    val t = typesOf("let x = one", ctx)

    assert(t == HashMap(
      "x" -> "Int"
    ))
  }

  test("type check abstraction") {
    val t = typesOf("let f = \\x -> x")

    assert(t == HashMap(
      "f" -> "t2 -> t2"
    ))
  }

  test("type check abstraction using a fn") {
    val ctx: Context = HashMap("iseven" -> Type.named("->", Type.named("Int"), Type.named("Bool")))
    val t = typesOf("let f = \\x -> iseven x", ctx)

    assert(t == HashMap(
      "f" -> "Int -> Bool"
    ))
  }

  test("type check complex abstr") {
    val t = typesOf("let f = \\x -> \\y -> y x")

    assert(t == HashMap(
      "f" -> "t6 -> (t6 -> t4) -> t4"
    ))
  }


  def typesOf(src: String, context: Context = HashMap.empty) = {
    val unifier = Unifier()
    val tc = Typechecker(unifier)

    val parsed = Parser(src).get
    parsed.foldRight(HashMap.empty)((decl, map: HashMap[String, String]) => {
      decl match {
        case Declaration.Let(name, expr) =>
          val t = unifier.freshVar()
          tc.typecheckExpr(expr, t, context).toOption.get

          // val res = generalize(context, s(m))
          val tres = unifier.resolve(t)
          map.updated(name, pprint(tres))
      }
    })

  }
