package ru.kharvd.encryptor

import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, _}
import ru.kharvd.encryptor.PermutationGenerators._

class PermutationTest extends FunSuite with GeneratorDrivenPropertyChecks with ShouldMatchers {

  test("Identity permutation doesn't permute anything") {
    forAll(identityPermutationGenerator(0, 100), arbitrary[Vector[Int]]) { (p: Permutation, l: Vector[Int]) =>
      whenever(l.size >= p.size) {
        p.permute(l) should equal(l)
      }
    }
  }

  test("Inverse permutation") {
    forAll(permutationGenerator(0, 100), arbitrary[Vector[Int]]) { (p: Permutation, l: Vector[Int]) =>
      whenever(l.size >= p.size) {
        p.inverse.permute(p.permute(l)) should equal(l)
        p.permute(p.inverse.permute(l)) should equal(l)
      }
    }
  }

  test("Permutation composition") {
    forAll(permutationGenerator(0, 100), arbitrary[Vector[Int]]) { (p: Permutation, l: Vector[Int]) =>
      whenever(l.size >= p.size) {
        (p.inverse * p).permute(l) should equal(l)
        (p * p.inverse).permute(l) should equal(l)
      }
    }
  }

  test("Examples") {
    val perm = Permutation(3, 0, 2, 1)
    assertResult("hapl")(perm.permute("alph").mkString)
  }
}
