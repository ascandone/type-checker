package lambda

import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite:
  test("let decl") {
    val result = Declaration.Let(
      name = "x",
      value = Expr.Var("e")
    )

    assert(Parser("let x = e").get === List(result))
  }

  test("simple variable expr") {
    assertParseExpr("z", Expr.Var("z"))
  }

  test("appl") {
    assertParseExpr("f x",
      Expr.App(
        f = Expr.Var("f"),
        x = Expr.Var("x"))
    )
  }

  test("appl nested") {
    assert(Parser("let x = a b c").get === Parser("let x = (a b) c").get)
  }

  test("appl in lambda") {
    assertParseExpr("\\ x -> a b",
      Expr.Abs(
        param = "x",
        body = Expr.App(
          f = Expr.Var("a"),
          x = Expr.Var("b")
        )
      )
    )
  }

  test("abstr") {
    assertParseExpr("\\ x -> y",
      Expr.Abs(
        param = "x",
        body = Expr.Var("y")
      )
    )
  }

  test("nested abstr sugar") {
    assertParseExpr("\\ x y -> y",
      Expr.Abs(
        param = "x",
        body = Expr.Abs(
          param = "y",
          body = Expr.Var("y")
        )
      )
    )
  }

  test("many let") {
    val result = List(
      Declaration.Let(
        name = "x",
        value = Expr.Var("a")
      ),
      Declaration.Let(
        name = "y",
        value = Expr.Var("b")
      ),
    )


    assert(Parser("let x = a let y = b").get === result)
  }

  test("let fn sugar") {
    val result = List(
      Declaration.Let(
        name = "f",
        value = Expr.Abs(
          param = "x",
          body = Expr.Abs(
            param = "y",
            body = Expr.Var("y")
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
