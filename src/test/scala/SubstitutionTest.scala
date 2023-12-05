import org.scalatest.funsuite.AnyFunSuite
import unify.*

import scala.collection.immutable.HashMap;

class SubstitutionTest extends AnyFunSuite:
  test("Substitution should not change types without vars") {
    val s: Substitution = HashMap(
      "a" -> MonoType.Var("b"),
    )

    val t = MonoType.concrete("List", MonoType.concrete("Bool"))

    assert(applySubstitution(s, t) === t)
  }

  test("Monotypes should substitute vars") {
    val s: Substitution = HashMap(
      "a" -> MonoType.concrete("Bool"),
    )

    val t = MonoType.concrete("Apply", MonoType.Var("a"), MonoType.Var("b"))

    val result = MonoType.concrete("Apply", MonoType.concrete("Bool"), MonoType.Var("b"))

    assert(applySubstitution(s, t) === result)
  }

  test("Mutually exclusive substs") {
    val s1: Substitution = HashMap(
      "a" -> MonoType.concrete("X"),
    )

    val s2: Substitution = HashMap(
      "b" -> MonoType.concrete("Y"),
    )

    val result: Substitution = HashMap(
      "a" -> MonoType.concrete("X"),
      "b" -> MonoType.concrete("Y"),
    )

    assert(composeSubstitution(s1, s2) === result)
  }

  test("Substs on same sym") {
    val s1: Substitution = HashMap(
      "a" -> MonoType.concrete("X"),
    )

    val s2: Substitution = HashMap(
      "a" -> MonoType.concrete("Y"),
    )

    val result: Substitution = HashMap(
      "a" -> MonoType.concrete("X"),
    )

    assert(composeSubstitution(s1, s2) === result)
  }

  test("Concatenated substs") {
    val s1: Substitution = HashMap(
      "a" -> MonoType.Var("b"),
    )

    val s2: Substitution = HashMap(
      "b" -> MonoType.concrete("X"),
    )

    val result: Substitution = HashMap(
      "a" -> MonoType.concrete("X"),
      "b" -> MonoType.concrete("X"),
    )

    assert(composeSubstitution(s1, s2) === result)
  }

