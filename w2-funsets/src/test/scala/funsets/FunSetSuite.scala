package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
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
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
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
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("element of s1 is contained in s1 ? s1") {
    new TestSets {
      val sc = intersect(s1, s1)
      assert(contains(sc, 1), "1 ? s1 ? s1")
      assert(!contains(sc, 2), "2 ? s1 ? s1")
      assert(!contains(sc, 3), "3 ? s1 ? s1")
    }
  }

  test("no element is contained in s1 ? s2") {
    new TestSets {
      val sc = intersect(s1, s2)
      assert(!contains(sc, 1), "1 ? s1 ? s2")
      assert(!contains(sc, 2), "2 ? s1 ? s2")
      assert(!contains(sc, 3), "3 ? s1 ? s2")
    }
  }

  test("(s1 ? s2) ? (s2 ? s3) contains the element from s2") {
    new TestSets {
      val s = intersect(union(s1, s2), union(s2, s3))
      assert(!contains(s, 1), "1 ? (s1 ? s2) ? (s2 ? s3)")
      assert(contains(s, 2), "2 ? (s1 ? s2) ? (s2 ? s3)")
      assert(!contains(s, 3), "3 ? (s1 ? s2) ? (s2 ? s3)")
    }
  }

  test("s1 \\ s1 contains the element from s1") {
    new TestSets {
      val s = diff(s1, s1)
      assert(!contains(s, 1), "1 ? s1 \\ s2")
      assert(!contains(s, 2), "2 ? s1 \\ s2")
      assert(!contains(s, 3), "3 ? s1 \\ s2")
    }
  }

  test("s1 \\ s2 contains the element from s1 but not s2") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "1 ? s1 \\ s2")
      assert(!contains(s, 2), "2 ? s1 \\ s2")
      assert(!contains(s, 3), "3 ? s1 \\ s2")
    }
  }

  test("filtered sets") {
    new TestSets {
      val isOdd = (x: Int) => x % 2 == 0
      val filteredSet = filter(union(union(s1, s2), s3), isOdd)
      assert(!contains(filteredSet, 1), "1 ? filteredSet")
      assert(contains(filteredSet, 2), "2 ? filteredSet")
      assert(!contains(filteredSet, 3), "3 ? filteredSet")
    }
  }

  test("forall") {
    val s = (x: Int) => -1000 <= x && x <= 1000
    assert(forall(s, (x: Int) => -1999 < x))
  }

  test("negated forall") {
    val s = (x: Int) => -1000 <= x && x <= 1000
    assert(!forall(s, (x: Int) => x < 0))
  }

  test("exists") {
    val s = (x: Int) => -1000 <= x && x <= 1000
    assert(exists(s, (x: Int) => x == 999))
  }

  test("negated exists") {
    val s = (x: Int) => -1000 <= x && x <= 1000
    assert(!exists(s, (x: Int) => x > 1000))
  }

  test("map") {
    val s: Set = (x: Int) => List[Int](1, 2, 3, 4) contains x
    val s2: Set = map(s, (x: Int) => x * 2)
    assert(!contains(s2, 1))
    assert(contains(s2, 2))
    assert(!contains(s2, 3))
    assert(contains(s2, 4))
    assert(!contains(s2, 5))
    assert(contains(s2, 6))
    assert(!contains(s2, 7))
    assert(contains(s2, 8))
  }
}
