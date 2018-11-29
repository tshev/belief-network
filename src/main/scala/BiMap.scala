import scala.collection.mutable

class BiMap(var forward: mutable.HashMap[String, Int], var backward: mutable.HashMap[Int, String]) {
  def put(key: String, value: Int) : Unit = {
    this.forward.put(key, value)
    this.backward.put(value, key)
  }

  def remove(key: String, value: Int) : Unit = {
    this.forward.remove(key)
    this.backward.remove(value)
  }

  def get(key: String) : Option[Int] = {
    this.forward.get(key)
  }

  def get(key: Int) : Option[String] = {
    this.backward.get(key)
  }

  def apply(key: String) : Int = {
    this.forward(key)
  }

  def apply(key: Int) : String = {
    this.backward(key)
  }

  def size: Int = this.forward.size
}

object BiMap {
  def empty(): BiMap = {
    val forward = scala.collection.mutable.HashMap.empty[String, Int]
    val backward = scala.collection.mutable.HashMap.empty[Int, String]
    return new BiMap(forward, backward)
  }
}
