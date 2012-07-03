package insane
package utils

class RingBuffer[T](val size: Int) {

  private[this] var buffer: List[T] = Nil
  var bufferSize  = 0;

  def append(t: T) {
    bufferSize += 1
    buffer      = t :: buffer

    if (bufferSize > size) {
      buffer = buffer.dropRight(1)
    }
  }

  def contents = buffer
}
