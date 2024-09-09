package typechecker_mut

import typechecker_mut.Type.*

def pprint(t: Type): String =
  t match
    case Var(v) => "t" + v.toString
    case Named("->", left :: right :: Nil) =>
      val sb = StringBuilder()
      left match
        case Named("->", _) =>
          sb ++= "("
          sb ++= pprint(left)
          sb ++= ")"
        case _ =>
          sb ++= pprint(left)

      sb ++= " -> "
      sb ++= pprint(right)
      sb.toString()
    case Named(c, args) =>
      val sb = StringBuilder()
      sb ++= c
      for arg <- args do
        sb ++= " "
        arg match
          case Named(_, _) =>
            sb ++= "("
            sb ++= pprint(arg)
            sb ++= ")"
          case Var(_) =>
            sb ++= pprint(arg)
      sb.toString()


val OFFSET = 'a'.toInt
val MAX = 'z'.toInt
