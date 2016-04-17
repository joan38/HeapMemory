package com.goyeau.heapmemory

import scala.collection.mutable

class HeapMemory(val heapSize: Int) {

  private val freeMemory = mutable.TreeSet(FreeBlock(0, heapSize))(Ordering.by(_.size))

  /**
    * Allocate a freeMemory block for the given size.
    *
    * @param numBlocksRequired
    * @return
    */
  def allocate(numBlocksRequired: Int): AllocatedBlock = {
    require(numBlocksRequired > 0, "numBlocksRequired should be greater than 0")

    val smallestFreeBlockFitting = freeMemory find
      (_.size >= numBlocksRequired) getOrElse
      (throw new OutOfMemoryError())

    val newAllocatedBlock = AllocatedBlock(
      smallestFreeBlockFitting.start,
      numBlocksRequired
    )

    freeMemory -= smallestFreeBlockFitting

    newAllocatedBlock.setPrevious(smallestFreeBlockFitting.getPrevious)
    newAllocatedBlock.setNext {
      if (numBlocksRequired < smallestFreeBlockFitting.size) {
        val remainingFreeBlock = FreeBlock(
          newAllocatedBlock.start + newAllocatedBlock.size,
          smallestFreeBlockFitting.size - newAllocatedBlock.size
        )
        remainingFreeBlock.setNext(smallestFreeBlockFitting.getNext)

        freeMemory += remainingFreeBlock
        remainingFreeBlock
      }
      else smallestFreeBlockFitting.getNext
    }

    newAllocatedBlock
  }

  /**
    * Free the freeMemory for the given block
    *
    * @param block
    * @return
    */
  def free(block: AllocatedBlock) = (block.getPrevious, block.getNext) match {
    // Previous and next blocks are fee blocks. Group them with the freed block.
    case (previous: FreeBlock, next: FreeBlock) =>
      freeMemory -= previous
      freeMemory -= next
      freeMemory += FreeBlock(previous.start, previous.size + block.size + next.size)

    // Previous blocks is a free blocks. Group it with the freed block.
    case (previous: FreeBlock, _) =>
      freeMemory -= previous
      freeMemory += FreeBlock(previous.start, previous.size + block.size)

    // Next blocks is a free blocks. Group it with the freed block.
    case (_, next: FreeBlock) =>
      freeMemory -= next
      freeMemory += FreeBlock(block.start, block.size + next.size)

    // Previous and next are allocated blocks. We just add a free block instead.
    case _ =>
      freeMemory += FreeBlock(block.start, block.size)
  }
}