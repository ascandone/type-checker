package typechecker

import typechecker.Type.*

private def pprintHelper(t: Type): String =
  t match
    case Var(v) => "t" + v.toString
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
