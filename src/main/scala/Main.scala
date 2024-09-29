import lambda._
import typechecker._

import scala.collection.immutable.HashMap
import scala.io.Source

@main
def main(): Unit =
  val l = Source.fromResource("source.txt").mkString("")
  val parsed = Parser(l)
  val declarations = parsed.getOrElse {
    throw Error(s"Parsing error:\n$parsed")
  }

  var context: Context = HashMap(
    "one" -> (Set.empty, Type.named("Int")),
    "true" -> (Set.empty, Type.named("Bool")),
    "ifThenElse" -> (Set(0), Type.named("->",
      Type.named("Bool"),
      Type.named("->" ,
        Type.Var(0),
        Type.named("->" ,
          Type.Var(0),
          Type.Var(0))))),
    "succ" -> (Set.empty, Type.named("->",
      Type.named("Int"),
      Type.named("Int"))),
    "empty" -> (
        Set(0),
        Type.named("List", Type.Var(0))
      ),
    "head" -> (
      Set(0),
      Type.named("->",
        Type.named("List", Type.Var(0)),
        Type.named("Maybe", Type.Var(0)),
      )
    ),
  )

  val tc = Typechecker()
  val out = tc.typecheckDeclarations(declarations, context)
  out match
    case Left(err) => println(err)
    case Right(out) =>
      for (name, (_, res)) <- (out -- context.keys).toList.sortBy(p => p._1)
      do println(s"$name :: ${pprint(res)}")
