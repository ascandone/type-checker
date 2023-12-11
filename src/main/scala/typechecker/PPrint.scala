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
private def pprintRec(polyType: PolyType): String =
  polyType match
    case PolyType.ForAll(_, p) => pprintRec(p)
    case PolyType.Mono(m) => pprint(m)


def pprint(polyType: PolyType): String =
  val n = Normalize().run(polyType)
  pprintRec(n)

val OFFSET = 'a'.toInt
val MAX = 'z'.toInt

class Normalize:
  private var currentIndex = 0

  private def freshIdent(): String =
    // TODO handle overflow over 'z'
    val index = currentIndex + OFFSET
    currentIndex += 1
    index.toChar.toString

  def run(polyType: PolyType): PolyType =
    polyType match
      case PolyType.Mono(t) => t
      case PolyType.ForAll(name, p) =>
        val rec = run(p)
        val newName = freshIdent()
        val s = Substitution.fromEntries(
          name -> MonoType.Var(newName)
        )
        PolyType.ForAll(newName, s(rec))
