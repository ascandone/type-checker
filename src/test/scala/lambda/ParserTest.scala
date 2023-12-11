package lambda

import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite:
  test("let decl") {
    val result = Declaration.Let(
      name = "x",
      value = Expr.Variable("e")
    )

    assert(Parser("let x = e").get === List(result))
  }

  test("simple variable expr") {
    assertParseExpr("z", Expr.Variable("z"))
  }

  test("appl") {
    assertParseExpr("f x",
      Expr.App(
        f = Expr.Variable("f"),
        x = Expr.Variable("x"))
    )
  }

  test("abstr") {
    assertParseExpr("\\ x -> y",
      Expr.Fn(
        param = "x",
        body = Expr.Variable("y")
      )
    )
  }

  test("nested abstr sugar") {
    assertParseExpr("\\ x y -> y",
      Expr.Fn(
        param = "x",
        body = Expr.Fn(
          param = "y",
          body = Expr.Variable("y")
        )
      )
    )
  }

  test("many let") {
    val result = List(
      Declaration.Let(
        name = "x",
        value = Expr.Variable("a")
      ),
      Declaration.Let(
        name = "y",
        value = Expr.Variable("b")
      ),
    )


    assert(Parser("let x = a let y = b").get === result)
  }

  test("let fn sugar") {
    val result = List(
      Declaration.Let(
        name = "f",
        value = Expr.Fn(
          param = "x",
          body = Expr.Fn(
            param = "y",
            body = Expr.Variable("y")
          )
        )
      ),
    )


    assert(Parser("let f x y = y").get === result)
  }

  def assertParseExpr(src: String, expr: Expr): Unit = {
    val result = Declaration.Let(
      name = "x",
      value = expr
    )

    val out = Parser(s"let x = $src")

    assert(out.get === List(result))
  }
