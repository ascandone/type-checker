package lambda

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

object Parser extends RegexParsers with PackratParsers {
  private val reserved = List("let", "in")

  private def fnSugar(params: List[String], body: Expr) =
    params.foldRight(body)(Expr.Abs.apply)

  private lazy val identifier: PackratParser[String] =
    """[a-zA-Z]+""".r.filter { x => !reserved.contains(x) }

  private lazy val variable: PackratParser[Expr] =
    identifier ^^ { Expr.Var.apply }

  private lazy val abstraction: PackratParser[Expr] =
    "\\" ~> identifier.+ ~ "->" ~ expression ^^ {
      case params ~ _ ~ e => fnSugar(params, e)
    }

  private lazy val letExpr: PackratParser[Expr] =
    "let" ~> identifier.+ ~ "=" ~ expression ~ "in" ~ expression ^^ {
      case (name :: params) ~ _ ~ value ~ _ ~ body =>
        Expr.Let(name, fnSugar(params, value), body)
      case _ => throw java.lang.Error()
    }

  private lazy val application: PackratParser[Expr] =
      expression ~ expression ^^ { case e1 ~ e2 => Expr.App(e1, e2) }

  private lazy val applicable: PackratParser[Expr] =
    variable | "(" ~> expression <~ ")"

  private lazy val applicationChain: PackratParser[Expr] =
    applicable.+ ^^ {
      case x :: Nil => x
      case f :: xs => xs.foldLeft(f)((f, x) => Expr.App(f, x))
      case _ => throw java.lang.Error("Unreachable")
    }

  private lazy val expression: PackratParser[Expr] =
      abstraction | letExpr | applicationChain

  private lazy val letDeclaration: PackratParser[Declaration] =
      "let" ~> identifier.+ ~ "=" ~ expression ^^ {
        case (name :: params) ~ _ ~ value => Declaration.Let(name, fnSugar(params, value))
        case _ => throw java.lang.Error()
      }

  private lazy val declarations: PackratParser[List[Declaration]] =
    letDeclaration.*

  def apply(input: String): lambda.Parser.ParseResult[List[Declaration]] =
    parseAll(declarations, input)
}