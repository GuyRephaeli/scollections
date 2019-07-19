package scollections

import scala.util.Random


/**
  * An API with added functionality for sequences
  */
object Scollections {
  def main(args: Array[String]): Unit = {
    println("hello maagar")
  }

  /**
    * Determines whether a sequence s is a palindrome or not, e.g:
    * isPalindrome(Seq(1,1,2,2,2,3,1)) = False
    *
    * @param s - the sequence to examine
    * @tparam A - the type of the sequence elements
    * @return - a Boolean representing if s is a palindrome
    */
  def isPalindrome[A](s: Seq[A]): Boolean = {
    s.reverse.equals(s)
  }

  /**
    * Eliminate repeating elements in a sequence by replacing them with a single instance, e.g:
    * compress(Seq(1,1,2,2,2,3,1)) = Seq(1,2,3,1)
    *
    * @param s - the sequence to be compressed
    * @tparam A - the type of the sequence elements
    * @return - a repetition-less version of s
    */
  def compress[A](s: Seq[A]): Seq[A] = {
    s.pack.map(_.head)
  }

  /**
    * Replace repeating elements in a sequence with a sequence of themselves, e.g:
    * pack(Seq(1,1,2,2,2,3,1)) = Seq(Seq(1,1),Seq(2,2,2),Seq(3),Seq(1))
    *
    * @param s - the sequence to be packed
    * @tparam A - the type of the sequence elements
    * @return - a sequence of all repetitions in s
    */
  def pack[A](s: Seq[A]): Seq[Seq[A]] = {
    if (s.isEmpty) {
      return Seq.empty
    }

    val breakIndices = 0 +:
      s.take(s.size - 1)
        .zip(s.tail)
        .zipWithIndex
        .collect { case ((v1, v2), index)
          if v1 != v2 => index + 1
        }

    breakIndices
      .zip(breakIndices.tail :+ s.size)
      .map { case (startAt, stopAt) => s.slice(startAt, stopAt) }
  }

  /**
    * Replace repeating elements in a sequence s with the element value and the number of repetitions, e.g:
    * encode(Seq(1,1,2,2,2,3,1)) = Seq((2,1),(3,2),(1,3),(1,1))
    *
    * @param s - the sequence with (or without) repeating elements
    * @tparam A - the type of the sequence elements
    * @return - a sequence with tuples: (n, e), where e is an element from s
    *         and n is the number of times it appears in a row
    */
  def encode[A](s: Seq[A]): Seq[(Int, A)] = {
    s.pack.map(x => (x.size, x.head))
  }

  /**
    * Invert the operation of encode - replace element values and a number with actual repetition of the element, e.g:
    * decode(Seq((2,1),(3,2),(1,3),(1,1))) = Seq(1,1,2,2,2,3,1)
    *
    * @param pairs - the pairs of elements values and the number of repetitions
    * @tparam A - the type of the sequence elements
    * @return - a sequence with repeating elements as specified in pairs
    */
  def decode[A](pairs: Seq[(Int, A)]): Seq[A] = {
    val containsConsecutiveValues: Boolean = pairs
      .map { case (_, x) => x }
      .pack
      .exists(x => x.size > 1)

    if (containsConsecutiveValues) {
      throw new IllegalArgumentException("Encoded sequence cannot have two consecutive tuples with the same value")
    }

    pairs.flatMap { case (repetitions, x) => Seq.fill(repetitions)(x) }
  }

  /**
    * Duplicate each element in a sequence s, e.g:
    * duplicate(Seq(1,1,2,2,2,3,1)) = Seq(1,1,1,1,2,2,2,2,2,2,3,3,1,1)
    *
    * @param s - the sequence who's elements are to be duplicated
    * @tparam A - the type of the sequence elements
    * @return - a sequence with the elements of s, only each element is repeated
    */
  def duplicate[A](s: Seq[A]): Seq[A] = {
    s.flatMap(e => Seq(e, e))
  }

  /**
    * Drop every n'th element from the sequence s, e.g:
    * drop(Seq(1,1,2,2,2,3,1), 2) = Seq(1,2,2,1)
    *
    * @param s - the sequence from which elements are to be dropped
    * @param n - the number specifying which elements to drop
    * @tparam A - the type of the sequence elements
    * @return - a sequence that is identical to s except for the dropped elements
    */
  def drop[A](s: Seq[A], n: Int): Seq[A] = {
    require(n > 0, "n must be a positive integer")

    s.zipWithIndex
      .filterNot { case (_, index) => ((index + 1) % n) == 0 }
      .map { case (value, _) => value }
  }

  /**
    * Split a sequence s to two sequences in a specified location n, e.g:
    * split(Seq(1,1,2,2,2,3,1), 2) = (Seq(1,1),Seq(2,2,2,3,1))
    *
    * @param s - the sequence to be split
    * @param n - the location to split s at
    * @tparam A - the type of the sequence elements
    * @return - a tuple with the two sequences created by splitting s at n
    */
  def splitAt[A](s: Seq[A], n: Int): (Seq[A], Seq[A]) = {
    if (n > s.size || n < 0) {
      throw new IndexOutOfBoundsException("Splitting index must be >= 0 and <= the sequence length")
    }
    val splitWithIndices = s.zipWithIndex
      .partition { case (_, index) => index < n }
    val (prefix, suffix) = splitWithIndices

    val res = Seq(prefix, suffix)
      .map(_
        .map { case (value, _) => value })

    (res.head, res.last)
  }

  /**
    * Split a sequence s to several sequences based on the positions of elements equal to a.
    * The resulted sequences will not include a, e.g:
    * splitBy(Seq(1,1,2,2,2,3,1), 2) = Seq(1,1),Seq(),Seq(),Seq(3,1))
    *
    * @param s - the sequence to be split
    * @param a - the element by which the splitting is performed
    * @tparam A - the type of the sequence elements
    * @return - a sequence of sequences that are the splits of s when split by a
    */
  def splitBy[A](s: Seq[A], a: A): Seq[Seq[A]] = {
    val indicesOfA = -1 +:
      s.zipWithIndex
        .collect { case (value, index)
          if value == a => index
        }

    indicesOfA
      .zip(indicesOfA.tail :+ s.size)
      .map { case (startAt, stopAt) => s.slice(startAt + 1, stopAt) }
  }

  /**
    * Shift all elements of a sequence n steps to the left (or |n| steps to the right if n is negative).
    * It is a cyclic operation so elements pushed "out" of the sequence are appended back to the end of the sequence
    * (or to the beginning if n is negative), e.g:
    * rotateLeft(Seq(1,1,2,2,2,3,1), 2) = Seq(2,2,2,3,1,1,1)
    *
    * @param s - the sequence its elements are to be pushed
    * @param n - the number of steps left the elements of s are o be pushed
    * @tparam A - the type of the sequence elements
    * @return - a sequence containing all elements of s only pushed n steps to the left
    */
  def rotateLeft[A](s: Seq[A], n: Int): Seq[A] = {
    if (s.isEmpty) {
      return s
    }
    val signedD = n % s.size
    val d = if (n >= 0) {
      signedD
    } else {
      signedD + s.size
    }
    val (prefix, suffix) = s.splitAt(d)
    suffix ++ prefix
  }

  /**
    * Sample n random elements of a sequence s, e.g:
    * pick(Seq(1,1,2,2,2,3,1), 2) = Seq(2,1) or Seq(2,2) or Seq(1,3) etc.
    *
    * @param s - the sequence being sampled
    * @param n - the number of samples to take from s
    * @tparam A - the type of the sequence elements
    * @return - a sequence containing all n samples from s
    */
  def pick[A](s: Seq[A], n: Int): Seq[A] = {
    require(n >= 0 && n <= s.size, "Cannot pick less than 0 numbers or more than all the numbers")

    Random.shuffle(s).take(n)
  }


  /**
    * An extension class for Seq with the functionality of Scollections
    * This class holds no logic - only calls to the API functions
    *
    * @param s - the extended sequence
    * @tparam A - the type parameter of the sequence
    */
  implicit class ScollectionsExtension[A](s: Seq[A]) {
    def isPalindrome: Boolean = {
      Scollections.isPalindrome[A](s)
    }

    def compress: Seq[A] = {
      Scollections.compress[A](s)
    }

    def pack: Seq[Seq[A]] = {
      Scollections.pack[A](s)
    }

    def encode: Seq[(Int, A)] = {
      Scollections.encode[A](s)
    }

    def duplicate: Seq[A] = {
      Scollections.duplicate[A](s)
    }

    def drop(n: Int): Seq[A] = {
      Scollections.drop[A](s, n)
    }

    def splitAt(n: Int): (Seq[A], Seq[A]) = {
      Scollections.splitAt[A](s, n)
    }

    def splitBy(a: A): Seq[Seq[A]] = {
      Scollections.splitBy[A](s, a)
    }

    def rotateLeft(n: Int): Seq[A] = {
      Scollections.rotateLeft[A](s, n)
    }

    def pick(n: Int): Seq[A] = {
      Scollections.pick[A](s, n)
    }
  }
}
