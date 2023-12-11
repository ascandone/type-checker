package typechecker

import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.HashMap

class MTest extends AnyFunSuite:
  test("type check mono variable bound in context") {
    val i = freshIdent()

    val e = Expr.Var("one")
    val ctx: Context = HashMap(
      "one" -> MonoType.concrete("Int")
    )

    assert(algorithmM(ctx, e, MonoType.Var(i)).toOption.get == Substitution.fromEntries(
      i -> MonoType.concrete("Int")
    ))
  }
