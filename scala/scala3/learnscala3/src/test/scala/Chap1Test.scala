import org.scalatest.funsuite.AnyFunSuite

class SetSpec extends AnyFunSuite:

    test("An empty Set should have size 0") {
      assert(Set.empty.size == 0)
    }

    test("Invoking head on an empty Set should fail") {
      assertThrows[NoSuchElementException] {
        Set.empty.head
      }
    }
