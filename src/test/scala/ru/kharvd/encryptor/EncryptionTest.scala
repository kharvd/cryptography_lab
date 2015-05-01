package ru.kharvd.encryptor

import org.scalacheck.Arbitrary._
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import ru.kharvd.encryptor.PermutationGenerators._
import ru.kharvd.encryptor.TranspositionEncryptor._

class EncryptionTest extends FunSuite with GeneratorDrivenPropertyChecks with ShouldMatchers {

  test("Decrypt should be inverse for encrypt") {
    forAll(arbitrary[String], permutationGenerator(1, 100)) { (s: String, k: Permutation) =>
      decrypt(encrypt(s, k), k).trim should equal(s)
      encrypt(decrypt(s, k), k).trim should equal(s)
    }
  }
}
