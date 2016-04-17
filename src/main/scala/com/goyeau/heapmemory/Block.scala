package com.goyeau.heapmemory

/**
  * Created by Joan on 17/04/2016.
  */
sealed trait Block {
  private var previous: Block = _
  def getPrevious = previous
  def setPrevious(block: Block) = {
    previous = block
    if (block != null) block.next = this
  }

  private var next: Block = _
  def getNext = next
  def setNext(block: Block) = {
    next = block
    if (block != null) block.previous = this
  }

  def start: Int
  def size: Int
}

case class FreeBlock(start: Int, size: Int) extends Block

case class AllocatedBlock(start: Int, size: Int) extends Block with DataBlock {
  private var data: Array[Byte] = _
  private var freed = false

  private[heapmemory] def free() = freed = true

  def write(data: Array[Byte]): Boolean =
    if (!freed && data.length == size) {
      this.data = data
      true
    } else false

  def read(): Array[Byte] = data
}
