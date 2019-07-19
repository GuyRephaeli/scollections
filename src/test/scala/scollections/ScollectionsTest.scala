package scollections

class ScollectionsTest extends UnitTest {
  private val sequenceWithoutRepetitions = Seq(1, 2, 3)
  private val sequenceOfOneRepeatingElement = Seq(1, 1, 1)
  private val sequenceWithRepetitions = Seq(1, 1, 2, 3, 3, 3, 1)
  private val generalSequence = Seq(2, 3, 4)

  describe("isPalindrome - receives a sequence and check if it is a palindrome") {
    describe("when given an empty sequence") {
      it("returns true") {
        val s = Seq.empty
        val result = Scollections.isPalindrome(s)

        result mustBe true
      }
    }

    describe("when given a sequence of length 1") {
      it("returns true") {
        val s = Seq(1)
        val result = Scollections.isPalindrome(s)

        result mustBe true
      }
    }

    describe("when given a palindrome of length > 1") {
      it("returns true") {
        val s = Seq(1, 1)
        val result = Scollections.isPalindrome(s)

        result mustBe true
      }
    }

    describe("when given a non palindrome") {
      it("returns false") {
        val s = Seq(1, 2)
        val result = Scollections.isPalindrome(s)

        result mustBe false
      }
    }
  }

  describe("compress - eliminates repeating elements") {
    describe("when given an empty sequence") {
      it("returns an empty sequence of the same type") {
        val s = Seq.empty
        val result = Scollections.compress(s)

        result mustBe s
      }
    }

    describe("when given a sequence without repetitions") {
      it("returns the exact same sequence") {
        val s = sequenceWithoutRepetitions
        val result = Scollections.compress(s)

        result mustBe s
      }
    }

    describe("when given a sequence of one repeating element") {
      it("returns a sequence with a single instance of the element") {
        val s = sequenceOfOneRepeatingElement
        val expected = Seq(1)
        val result = Scollections.compress(s)

        result mustBe expected
      }
    }

    describe("when given a sequence with repetitions") {
      it("returns the sequence without repetitions") {
        val s = sequenceWithRepetitions
        val expected = Seq(1, 2, 3, 1)
        val result = Scollections.compress(s)

        result mustBe expected
      }
    }
  }

  describe("pack - wraps all repetitions of the same element with a sequence of their own") {
    describe("when given an empty sequence") {
      it("returns an empty sequence") {
        val s = Seq.empty
        val result = Scollections.pack(s)

        result mustBe empty
      }
    }

    describe("when given a sequence without repetitions") {
      it("returns the original sequence with every element wrapped in a sequence") {
        val s = sequenceWithoutRepetitions
        val expected = Seq(
          Seq(1),
          Seq(2),
          Seq(3))
        val result = Scollections.pack(s)

        result mustBe expected
      }
    }

    describe("when given a sequence with one repeating element") {
      it("returns a sequence containing a single sequence which is the original sequence") {
        val s = sequenceOfOneRepeatingElement
        val expected = Seq(s)
        val result = Scollections.pack(s)

        result mustBe expected
      }
    }

    describe("when given a sequence with repetitions") {
      it("returns a sequence of sequences with the same elements in the same order and the same repetitions") {
        val s = sequenceWithRepetitions
        val expected = Seq(
          Seq(1, 1),
          Seq(2),
          Seq(3, 3, 3),
          Seq(1))
        val result = Scollections.pack(s)

        result mustBe expected
      }
    }
  }

  describe("encode - replaces repetitions of the same element with a pair of the number of repetitions and the element value") {
    describe("when given an empty sequence") {
      it("returns an empty sequence") {
        val s = Seq.empty
        val result = Scollections.encode(s)

        result mustBe empty
      }
    }

    describe("when given a sequence without repetitions") {
      it("returns the original sequence with every element wrapped in a tuple with the number 1") {
        val s = sequenceWithoutRepetitions
        val expected = Seq(
          (1, 1),
          (1, 2),
          (1, 3))
        val result = Scollections.encode(s)

        result mustBe expected
      }
    }

    describe("when given a sequence with one repeating element") {
      it("returns a sequence containing a single tuple with the element and the length of the original sequence") {
        val s = sequenceOfOneRepeatingElement
        val expected = Seq((3, 1))
        val result = Scollections.encode(s)

        result mustBe expected
      }
    }

    describe("when given a sequence with repetitions") {
      it("returns a sequence of tuples with the same elements in the same order and the numbers of repetitions") {
        val s = sequenceWithRepetitions
        val expected = Seq(
          (2, 1),
          (1, 2),
          (3, 3),
          (1, 1))
        val result = Scollections.encode(s)

        result mustBe expected
      }
    }
  }

  describe("decode - inverting the operation of encode, i.e. replacing tuples of (n,e) by repeating e n times") {
    describe("when given an empty sequence") {
      it("returns an empty sequence") {
        val s = Seq.empty
        val result = Scollections.decode(s)

        result mustBe empty
      }
    }

    describe("when given a sequence with a single tuple (n,e)") {
      it("returns a sequence with e repeated n times") {
        val s = Seq((3, 1))
        val result = Scollections.decode(s)

        result mustBe sequenceOfOneRepeatingElement
      }
    }

    describe("when given a sequence of tuples with two consecutive tuples of the same element") {
      it("throws IllegalArgumentException") {
        val s = Seq(
          (1, 2),
          (3, 1),
          (2, 1),
          (1, 2))

        an [IllegalArgumentException] must be thrownBy Scollections.decode(s)
      }
    }

    describe("when given a sequence of tuples with no two consecutive tuples of the same element") {
      it("returns a sequence who's elements are repeated the number of times specified in the tuples and in the same order") {
        val s = Seq(
          (2, 1),
          (1, 2),
          (3, 3),
          (1, 1))
        val result = Scollections.decode(s)

        result mustBe sequenceWithRepetitions
      }
    }
  }

  describe("duplicate - repeat twice every element in a sequence") {
    describe("when given an empty sequence") {
      it("returns an empty sequence of the same type") {
        val s = Seq.empty
        val result = Scollections.duplicate(s)

        result mustBe s
      }
    }

    describe("when given a sequence with a single element repeated once or more") {
      it("returns a sequence of the same single element but twice the length") {
        val s = sequenceOfOneRepeatingElement
        val result = Scollections.duplicate(s)
        val expected = s ++ s

        result mustBe expected
      }
    }

    describe("when given a sequence") {
      it("returns the original sequence with every element appearing twice") {
        val s = Seq(1, 2)
        val result = Scollections.duplicate(s)
        val expected = Seq(1, 1, 2, 2)

        result mustBe expected
      }
    }
  }

  describe("drop - eliminate every n-th element in a sequence, where n is provided") {
    describe("when given an empty sequence and a strictly positive number") {
      it("returns an empty sequence of the same type") {
        val s = Seq.empty
        val n = 2
        val result = Scollections.drop(s, n)

        result mustBe s
      }
    }

    describe("when given a sequence and the number 1") {
      it("returns an empty sequence") {
        val s = generalSequence
        val n = 1
        val result = Scollections.drop(s, n)

        result mustBe empty
      }
    }

    describe("when given a sequence and the number 0") {
      it("throws IllegalArgumentException") {
        val s = generalSequence
        val n = 0

        an [IllegalArgumentException] must be thrownBy Scollections.drop(s, n)
      }
    }

    describe("when given a sequence and a negative number") {
      it("throws IllegalArgumentException") {
        val s = generalSequence
        val n = -1

        an [IllegalArgumentException] must be thrownBy Scollections.drop(s, n)
      }
    }

    describe("when given a sequence and a number larger than the sequence length") {
      it("returns the exact same sequence") {
        val s = generalSequence
        val n = s.length + 1
        val result = Scollections.drop(s, n)

        result mustBe s
      }
    }

    describe("when given a sequence and a number smaller or equal to the sequence length") {
      it("returns a sequence with the same elements without the elements in indices that are divided by the number") {
        val s = Seq(2, 3, 4, 5, 6)
        val n = 2
        val result = Scollections.drop(s, n)
        val expected = Seq(2, 4, 6)

        result mustBe expected
      }
    }
  }

  describe("splitAt - split a sequence to two sequences in a given position") {
    describe("when given an empty sequence and the number 0") {
      it("returns a tuple with two empty sequences") {
        val s = Seq.empty
        val n = 0
        val result = Scollections.splitAt(s, n)
        val expected = (s, s)

        result mustBe expected
      }
    }

    describe("when given an empty sequence and some number != 0") {
      it("throws IndexOutOfBoundsException") {
        val s = Seq.empty
        val n = 1

        an [IndexOutOfBoundsException] must be thrownBy Scollections.splitAt(s, n)
      }
    }

    describe("when given a sequence and a negative number") {
      it("throws IndexOutOfBoundsException") {
        val s = generalSequence
        val n = -1

        an [IndexOutOfBoundsException] must be thrownBy Scollections.splitAt(s, n)
      }
    }

    describe("when given a sequence and a number larger than the sequence length") {
      it("throws IndexOutOfBoundsException") {
        val s = generalSequence
        val n = s.length + 1

        an [IndexOutOfBoundsException] must be thrownBy Scollections.splitAt(s, n)
      }
    }

    describe("when given a sequence and the number 0") {
      it("returns an empty sequence and the original sequence") {
        val s = generalSequence
        val n = 0
        val result = Scollections.splitAt(s, n)
        val expected = (Seq.empty, s)

        result mustBe expected
      }
    }

    describe("when given a sequence and the length of the sequence") {
      it("returns the original sequence and an empty sequence") {
        val s = generalSequence
        val n = s.length
        val result = Scollections.splitAt(s, n)
        val expected = (s, Seq.empty)

        result mustBe expected
      }
    }

    describe("when given a sequence and a positive number smaller than the sequence length") {
      it(
        """returns two sequences who's contents concatenated is equal to the original sequence
          |and the length of the first sequence is equal to the specified number""".stripMargin) {
        val s = generalSequence
        val n = 2
        val result = Scollections.splitAt(s, n)
        val expected = (Seq(2, 3), Seq(4))

        result mustBe expected
      }
    }
  }

  describe(
    """splitBy - split a sequence by a specified element.
      |Symbols:
      |v = the provided value
      |n = the number of times v appears in the sequence""".stripMargin) {

    describe("when given an empty sequence and some value") {
      it("returns a sequence containing an empty sequence") {
        val s = Seq.empty
        val v = 1
        val result = Scollections.splitBy(s, v)

        result mustBe Seq(Seq.empty)
      }
    }

    describe("when given some sequence and some value that is not in that sequence") {
      it("returns a sequence containing the same sequence") {
        val s = generalSequence
        val v = 1
        val result = Scollections.splitBy(s, v)
        val expected = Seq(s)

        result mustBe expected
      }
    }

    describe("when given sequence containing only a single repeating value v and the specified value is v") {
      it("returns a sequence containing (n+1) empty sequences") {
        val s = sequenceOfOneRepeatingElement
        val v = sequenceOfOneRepeatingElement.head
        val n = sequenceOfOneRepeatingElement.length
        val result = Scollections.splitBy(s, v)
        val expected = Seq.fill(n+1)(Seq.empty)

        result mustBe expected
      }
    }

    describe("when given some sequence and some value v that appears in that sequence") {
      it(
        """returns a sequence of (n+1) sequences, non of which containing v, and are equal to the original sequence
          |when 'glued' together with v""".stripMargin) {
        val s = Seq(2, 2, 4, 2, 5, 2, 2, 3, 1, 2)
        val v = 2
        val result = Scollections.splitBy(s, v)
        val expected = Seq(
          Seq.empty,
          Seq.empty,
          Seq(4),
          Seq(5),
          Seq.empty,
          Seq(3, 1),
          Seq.empty)

        result mustBe expected
      }
    }
  }

  describe(
    """rotateLeft - shifts all elements of the sequence left in a cyclic manner, a specified number of times.
      |Symbols:
      |s = the sequence
      |n = the specified number of shifts left
      |l = the length of the sequence
      |d = (n mod l)""".stripMargin) {
    describe("when given an empty sequence and some number") {
      it("returns an empty sequence of the same type") {
        val s = Seq.empty
        val n = 1
        val result = Scollections.rotateLeft(s, n)

        result mustBe s
      }
    }

    describe("when given a sequence s s.t d = 0 ") {
      it("returns the exact same sequence") {
        val s = generalSequence
        val n = 0
        val result = Scollections.rotateLeft(s, n)

        result mustBe s
      }
    }

    describe("when given a sequence s and a number n s.t d != 0") {
      val s = generalSequence
      describe("and n is positive") {
        val n = 5
        val result = Scollections.rotateLeft(s, n)

        it("returns a sequence who's last d elements are equal to the first d elements of the original sequence" +
          "and the first (l-d) elements are equal to the last (l-d) elements of the original sequence") {
          val expected = Seq(4, 2, 3)

          result mustBe expected
        }

        describe("when applying the function again on the output with -n") {
          it("returns the original sequence") {
            val reverted = Scollections.rotateLeft(result, -n)

            reverted mustBe s
          }
        }
      }

      describe("and n is negative") {
        val n = -2
        val result = Scollections.rotateLeft(s, n)

        it("returns a sequence who's last d elements are equal to the first d elements of the original sequence" +
          "and the first (l-d) elements are equal to the last (l-d) elements of the original sequence") {
          val expected = Seq(3, 4, 2)

          result mustBe expected
        }
      }
    }
  }

  describe("pick - samples n different values from a sequence, where n is provided") {
    describe("when given a sequence and the number 0") {
      it("returns an empty sequence of the same type") {
        val s = generalSequence
        val n = 0
        val result = Scollections.pick(s, n)

        result mustBe empty
      }
    }

    describe("when given an empty sequence and a non-zero number") {
      it("throws IllegalArgumentException") {
        val s = Seq.empty
        val n = 1

        an [IllegalArgumentException] must be thrownBy Scollections.pick(s, n)
      }
    }

    describe("when given some sequence and a negative number") {
      it("throws IllegalArgumentException") {
        val s = generalSequence
        val n = -1

        an [IllegalArgumentException] must be thrownBy Scollections.pick(s, n)
      }
    }

    describe("when given some sequence and a number larger than the length of the sequence") {
      it("throws IllegalArgumentException") {
        val s = generalSequence
        val n = s.length + 1

        an [IllegalArgumentException] must be thrownBy Scollections.pick(s, n)
      }
    }

    describe("when given some sequence and a number equal to the length of the sequence") {
      it("returns s permutation of the original sequence") {
        val s = generalSequence
        val n = s.length
        val result = Scollections.pick(s, n)

        result.sorted mustBe s.sorted
      }
    }

    describe("when given some sequence and a number smaller than the length of the sequence") {
      it("returns a sequence of the specified length that is contained in the original sequence without order importance") {
        val s = Seq(2, 3, 4, 3, 7, 2, 1, 2, 4, 4)
        val n = 5
        val result = Scollections.pick(s, n)
        val diffSeq = s.sorted.diff(result.sorted)
        val diffSize = s.size - result.size

        diffSize mustBe diffSeq.size
      }
    }

    describe("when applied several times on sequence and a number smaller than the length of the sequence") {
      it("returns different sequences w.h.p") {
        val s = Seq(2, 3, 4, 3, 7, 2, 1, 2, 4, 4)
        val n = 5
        val results = Seq.fill(1000)(Scollections.pick(s, n))
        results.distinct mustNot have size 1
      }
    }
  }
}