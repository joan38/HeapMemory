import com.goyeau.heapmemory.{AllocatedBlock, Block, FreeBlock, HeapMemory}
import org.scalatest.{Matchers, PrivateMethodTester, WordSpec}

import scala.collection.mutable

class HeapMemoryTest extends WordSpec with Matchers with PrivateMethodTester {

  "Memory" when {

    trait TestBench {
      val heapSize = 100000
      val heapMemory = new HeapMemory(heapSize)

      def freeMemory = heapMemory invokePrivate PrivateMethod[mutable.TreeSet[Block]]('freeMemory)()
    }

    "should be empty at bootup" in new TestBench {
      freeMemory.toSet shouldBe Set(FreeBlock(0, heapSize))
    }

    "allocating a simple block" in new TestBench {
      val alloc = heapMemory.allocate(4)
      alloc shouldBe AllocatedBlock(0, 4)
      freeMemory.toSet shouldBe Set(FreeBlock(4, heapSize - 4))
    }

    "allocating a block of size 0" in new TestBench {
      intercept[IllegalArgumentException](heapMemory.allocate(0))
    }

    "allocating a block of negative size" in new TestBench {
      intercept[IllegalArgumentException](heapMemory.allocate(-5))
    }

    "allocating multiple blocks" should {
      "be ordered by size" in new TestBench {
        val b1 = heapMemory.allocate(4)
        val b2 = heapMemory.allocate(10)
        val b3 = heapMemory.allocate(8)
        val b4 = heapMemory.allocate(1)
        val b5 = heapMemory.allocate(3)
        heapMemory.free(b2)
        heapMemory.free(b4)

        freeMemory.toSeq shouldBe Seq(FreeBlock(22,1), FreeBlock(4,10), FreeBlock(26,99974))
      }
    }

    "full" should {
      "be completely allocated" in new TestBench {
        heapMemory.allocate(heapSize)
        freeMemory shouldBe empty
      }

      "not be possible to allocate anymore" in new TestBench {
        heapMemory.allocate(heapSize)
        intercept[OutOfMemoryError](heapMemory.allocate(1))
      }
    }
  }
}
