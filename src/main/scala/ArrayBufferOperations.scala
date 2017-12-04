import scala.collection.mutable.ArrayBuffer

object ArrayBufferOperations {
  def resize[T](data: ArrayBuffer[T], newSize: Int) : ArrayBuffer[T] = {
    val resizedData = new ArrayBuffer[T](newSize)
    val n = data.length.min(newSize)
    for (i <- 0 until n) {
      resizedData(i) = data(i)
    }
    resizedData
  }
}
