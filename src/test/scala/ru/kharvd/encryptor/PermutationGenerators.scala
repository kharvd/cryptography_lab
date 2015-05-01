package ru.kharvd.encryptor

import org.scalacheck.Gen

import scala.util.Random

object PermutationGenerators {
  def identityPermutationGenerator(min: Int, max: Int) =
    Gen.choose(min, max).map(n => Permutation(0 until n: _*))

  def permutationGenerator(min: Int, max: Int) =
    Gen.choose(min, max).map(n => Permutation(Random.shuffle(Vector(0 until n: _*)): _*))
}
