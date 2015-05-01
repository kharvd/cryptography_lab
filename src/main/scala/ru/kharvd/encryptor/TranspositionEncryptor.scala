package ru.kharvd.encryptor

object TranspositionEncryptor {

  /**
   * Encrypts the `message` using transposition cipher. 
   * If the string doesn't divide evenly into portions of the `key.size` size,
   * it will be padded with spaces.
   *
   * @param message Message to encrypt
   * @param key The encryption key (a permutation)
   * @return encrypted message
   * @see [[http://en.wikipedia.org/wiki/Transposition_cipher#Columnar_transposition}]]
   */
  def encrypt(message: String, key: Permutation): String = {
    if (key.size == 0) {
      throw new IllegalArgumentException("Key must not be empty")
    }

    message.grouped(key.size).map(permute(_, key)).mkString
  }

  /**
   * Decrypts a previously encrypted message using the supplied key
   *
   * @param cipher encrypted message
   * @param key a key used to encrypt the message
   * @return decrypted message
   */
  def decrypt(cipher: String, key: Permutation): String =
    encrypt(cipher, key.inverse)

  /**
   * Permutes the characters of the string according to the permutation.
   * If `block`'s length is less than the `key`'s, then `block` will 
   * be padded with spaces.
   *
   * @param block The text to be permuted
   * @param key The key represented by a permutation
   * @throws IllegalArgumentException if the key is empty
   */
  private def permute(block: String, key: Permutation): String = {
    if (key.size == 0) {
      throw new IllegalArgumentException("Key must not be empty")
    }

    val bl = block + List.fill(math.max(key.size - block.length, 0))(" ").mkString
    key.permute(bl).mkString
  }
}
