package lambda

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

object Parser extends RegexParsers with PackratParsers {
  private val reserved = List("let")

  private def fnSugar(params: List[String], body: Expr) =
    params.foldRight(body)(Expr.Fn.apply)

  private lazy val identifier: PackratParser[String] =
    """[a-zA-Z]+""".r.filter { x => !reserved.contains(x) }

  private lazy val variable: PackratParser[Expr] =
    identifier ^^ { Expr.Variable.apply }

  private lazy val abstraction: PackratParser[Expr] =
    "\\" ~> identifier.+ ~ "->" ~ expression ^^ {
      case params ~ _ ~ e => fnSugar(params, e)
    }

  private lazy val application: PackratParser[Expr] =
      expression ~ expression ^^ { case e1 ~ e2 => Expr.App(e1, e2) }

  private lazy val expression: PackratParser[Expr] =
      abstraction | application | variable | "(" ~> expression <~ ")" ^^ { identity }

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