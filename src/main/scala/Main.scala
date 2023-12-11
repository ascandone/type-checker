import lambda._
import typechecker._
import typechecker.monoToPoly

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
    "one" -> MonoType.concrete("Int"),
    "true" -> MonoType.concrete("Bool"),
    "succ" -> MonoType.concrete("->",
        MonoType.concrete("Int"),
        MonoType.concrete("Int")),
    "empty" -> PolyType.ForAll("a", MonoType.concrete("List", MonoType.Var("a"))),
    "head" -> PolyType.ForAll("a",
      MonoType.concrete("->",
        MonoType.concrete("List", MonoType.Var("a")),
        MonoType.concrete("Maybe", MonoType.Var("a"))))
  )

  for Declaration.Let(name, expr) <- declarations do
      val m = MonoType.Var(freshIdent())
      val s = algorithmM(context, expr, m).toOption.get

      val res = generalize(context, s(m))

      println(s"$name :: ${pprint(res)}")
      context = s(context).updated(name, res)
