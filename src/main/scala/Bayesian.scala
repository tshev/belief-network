import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer
import scala.xml.{Elem, XML}
import java.io.File

class MyArrayBuffer[T](val l: ArrayBuffer[T]) {
  def foo(): Unit = {
    println("foo")
  }
}

object Bayesian extends  App {
  override def main(args: Array[String]) {
    val nets = new ArrayBuffer[String]
    nets.append("/home/taras/code/my/bayesian_network/networks/animal.xml")
    nets.append("/home/taras/code/my/bayesian_network/networks/asia.xml")
    for (x <- nets) {
      println(x)
      val file = new File(x)
      val xml: Elem = XML.loadFile(x)
      val bayesianNetwork = BayesianNetwork.fromXML(xml)
      bayesianNetwork.classDict.
      write(f"${file.getName()}_probabilities.txt", bayesianNetwork.mkStringProbs)
      write(f"${file.getName()}_join_probabilities.txt", bayesianNetwork.mkStringJointProbs)
    }
  }

  def write(path: String, str: String): Unit = {
    val file = new File(path)
    val writer = new PrintWriter(file)
    writer.write(str)
    writer.close()
  }
}
