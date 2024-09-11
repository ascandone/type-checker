package typechecker

import typechecker.Type.*

import scala.annotation.tailrec

private def pprintRecordFields(fields: Map[String, Type]): String =
  fields.toList
    .sortBy((k, _) => k)
    .map((k, v) => f"$k ${pprintHelper(v)}")
    .mkString(", ")

@tailrec
private def pprintRecord(cons: Option[Type], fields: Map[String, Type]): String = cons match
  case Some(Record(cons, fields1)) => pprintRecord(cons, fields1 ++ fields)
  case _ =>
    val inner = fields.toList
        .sortBy((k, _) => k)
        .map((k, v) => f"$k ${pprintHelper(v)}")
        .mkString(", ")

    val closed = cons match
      case Some(t@Var(_)) => pprintHelper(t)
      case _ => ""

    f"[$inner]$closed"


private def pprintHelper(t: Type): String =
  t match
    case Var(v) => "t" + v.toString
    case Record(cons, fields) => pprintRecord(cons, fields)
    case Named("->", left :: right :: Nil) =>
      val sb = StringBuilder()
      left match
        case Named("->", _) =>
          sb ++= "("
          sb ++= pprintHelper(left)
          sb ++= ")"
        case _ =>
          sb ++= pprintHelper(left)

      sb ++= " -> "
      sb ++= pprintHelper(right)
      sb.toString()
    case Named(c, args) =>
      val sb = StringBuilder()
      sb ++= c
      for arg <- args do
        sb ++= " "
        arg match
          case Named(_, _) =>
            sb ++= "("
            sb ++= pprintHelper(arg)
            sb ++= ")"
          case Var(_) =>
            sb ++= pprintHelper(arg)
      sb.toString()

def pprint(t: Type): String =
  val scheme = generalise(t)
  val canonical = instantiate(Unifier(), scheme, t)
  pprintHelper(canonical)

val OFFSET = 'a'.toInt
val MAX = 'z'.toInt
