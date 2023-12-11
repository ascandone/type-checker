package typechecker

import scala.annotation.tailrec

def pprint(monoType: MonoType): String =
  monoType match
    case MonoType.Var(v) => v
    case MonoType.Concrete("->", left :: right :: Nil) =>
      val sb = StringBuilder()
      left match
        case MonoType.Concrete("->", _) =>
          sb ++= "("
          sb ++= pprint(left)
          sb ++= ")"
        case _ =>
          sb ++= pprint(left)

      sb ++= " -> "
      sb ++= pprint(right)
      sb.toString()
    case MonoType.Concrete(c, args) =>
      val sb = StringBuilder()
      sb ++= c
      for arg <- args do
        sb ++= " "
        arg match
          case MonoType.Concrete(_, _) =>
            sb ++= "("
            sb ++= pprint(arg)
            sb ++= ")"
          case MonoType.Var(_) =>
            sb ++= pprint(arg)
      sb.toString()

@tailrec
def pprint(polyType: PolyType): String =
  polyType match
    case PolyType.ForAll(_, p) => pprint(p)
    case PolyType.Mono(m) => pprint(m)
