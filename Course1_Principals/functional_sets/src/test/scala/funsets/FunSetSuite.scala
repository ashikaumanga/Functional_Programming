package funsets

import funsets.FunSets.FunSet
import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {

    def isPrime(n: Int): Boolean = ! ((2 until n-1) exists (n % _ == 0))

    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val setAllPositiveNums:FunSet = (x: Int) => x > 0
    val setEven:FunSet = (x: Int) => x%2 == 0
    val setOdd:FunSet = (x: Int) => x%2 != 0
    val setDivicibleBy4:FunSet = (x: Int) => x%4 == 0
    val setPrimes:FunSet = (x: Int) => isPrime(x)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect contains common elements of each set`: Unit = {
     new TestSets {
       val s = intersect(setEven, setDivicibleBy4)
       assert(contains(s, 4), "intersect")
       assert(contains(s, 8), "intersect")
       assert(!contains(s, 6), "intersect")
     }
  }

  @Test def `diff does not contain elements from second set`: Unit = {
    new TestSets {
      val s = diff(setAllPositiveNums, setEven)
      assert(contains(s, 1), "only odd")
      assert(contains(s, 3), "only odd")
      assert(!contains(s, 4), "not even numbers")
    }
  }

  @Test def `filtered set does not contain elements from filter condition`: Unit = {
    new TestSets {
      val s = filter(setEven, (x: Int) => x%10 != 0)
      assert(contains(s, 2), "has event numbers")
      assert(contains(s, 4), "only odd")
      assert(!contains(s, 10), "doesnt not have multiplications of 10")
      assert(!contains(s, 20), "doesnt not have multiplications of 10")
    }
  }
  //def forall(s: FunSet, p: Int => Boolean): Boolean

  @Test def `forall must satisfy the given func`: Unit = {
    new TestSets {
      assert(forall(setDivicibleBy4, (x: Int) => x%2 == 0), "divicible by 4 numbers are also divicible by 2")
      assert(!forall(setDivicibleBy4, (x: Int) => x%3 == 0), "not all divicible by 4 numbers are also divicible by 3")

    }
  }

  @Test def `exists `: Unit = {
    new TestSets {
      assert(exists(setEven, (x: Int) => x == 4), "4 is a even")
      assert(exists(setEven, (x: Int) => x == 4), "4 is a even")
      assert(!exists(setEven, (x: Int) => x==3), "3 is not a even")
      assert(exists(setOdd, (x: Int) => x%15==0), "odd numbers has atleast one number divided by 15")

    }
  }

  @Test def `map `: Unit = {
    new TestSets {
      val s = map(setEven, (x:Int) => x+1)
      assert(contains(s, 3), "adding one to even numbers makes it an odd")
      assert(contains(s, 5), "adding one to even numbers makes it an odd")
      assert(!contains(s, 6), "no even numbers")


    }
  }




  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
