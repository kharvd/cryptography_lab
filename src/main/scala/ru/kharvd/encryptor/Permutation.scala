package ru.kharvd.encryptor

class Permutation private(perm: Seq[Int]) extends (Int => Int) {

  /**
   * Order of the permutation 
   */
  val size = perm.size

  private val vec = Vector(perm: _*)

  /**
   * Inverse permutation 
   */
  lazy val inverse: Permutation = {
    val res: Array[Int] = Array.ofDim(size)
    vec.zipWithIndex.foreach { case (x, i) =>
      res(x) = i
    }

    Permutation(res: _*)
  }

  /**
   * Applies the permutation
   */
  def apply(i: Int): Int = if (0 until size contains i) vec(i) else i

  /**
   * Composes two permutations of the same order and returns their product
   */
  def *(other: Permutation): Permutation = {
    if (size != other.size) {
      throw new IllegalArgumentException("Cannot compose permutations of different size")
    }

    Permutation(Vector.tabulate(size)(i => apply(other(i))): _*)
  }

  /**
   * Permutes a sequence according to the permutation. The function 
   * only changes the order of first `n` elements of the sequence,
   * where `n` is the size of the permutation.
   */
  def permute[T](seq: Seq[T]): Seq[T] = {
    if (seq.size < size) {
      throw new IllegalArgumentException("Cannot permute a list whose size is less than permutation's size")
    }

    Vector.tabulate(seq.size)(i => seq(apply(i)))
  }
}

object Permutation {
  /**
   * Constructs a permutation from a list of numbers from `0` to `n - 1`,
   * where `n` is the size of the permutation.
   */
  def apply(xs: Int*) = new Permutation(xs)
}
