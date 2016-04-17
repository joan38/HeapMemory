package com.goyeau.heapmemory

/**
  * Created by Joan on 17/04/2016.
  */
trait DataBlock {
  def write(data: Array[Byte]): Boolean
  def read(): Array[Byte]
}
