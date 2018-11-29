import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.xml.{Elem, QNode}

class BayesianNetwork {
  class Node() {
    var nVars: Int = 0
    var ancestors = new ArrayBuffer[Int]
    var children = new ArrayBuffer[Int]
    var probabilities = new ArrayBuffer[Double]
    var conditionalProbabilities = new ArrayBuffer[Double]
  }
  object Node {
    def apply(nvars: Int): Node = {
      val node = new Node
      node.nVars = nvars
      node
    }
  }

  class Graph {
    var independent = new ArrayBuffer[Int]
    var path = new ArrayBuffer[Int]
    var jointProbabilities = new ArrayBuffer[Double]
    var nodes = new ArrayBuffer[Node]

    def apply(i: Int) : Node = {
      this.nodes.apply(i)
    }

    def extend(biMaps: ArrayBuffer[BiMap]): Unit = {
      biMaps.foreach(biMap => this.nodes.append(Node(biMap.size)))
    }

    def translateOffset(pStride: ArrayBuffer[Int],
                        pMap: mutable.HashMap[Int, Int],
                        deps: ArrayBuffer[Int],
                        pOffset: Int): (Int, Int) = {
      deps.foldLeft((1, 0))((res, dep) => {
        val pIndex: Int = pMap(dep)
        val dCount: Int =  nodes(pIndex).nVars
        val dOffset: Int = (pOffset / pStride(pIndex)) % dCount
        val newOffset = res._2 + (res._1 * dOffset)
        val newDStride = res._1 * dCount
        (newDStride, newOffset)
      })
    }

    def mkStringProbabilities(): String = {
      val stringBuilder: StringBuilder = new StringBuilder
      for ((node, i) <- nodes.zipWithIndex) {
        stringBuilder.append(f"#${classDict.apply(i)}\n")
        for (v <- 0 until node.nVars) {
          stringBuilder.append(s"${valueDict(i)(v)} = ${node.probabilities(v)}\n")
        }
      }
      stringBuilder.result
    }

    def mkStringJointProbabilities(): String = {
      val stringBuilder: StringBuilder = new StringBuilder
      for (index <- path) {
        stringBuilder.append(classDict(index)).append(",")
      }
      var sum: Double = 0.0
      for ((jointProb, i) <- jointProbabilities.zipWithIndex) {
        var offset = i
        for (index <- path) {
          val currentVar = offset % apply(index).nVars
          offset /= apply(index).nVars
          stringBuilder.append(f"${valueDict(index).apply(currentVar)}, ")
        }
        stringBuilder.append(jointProb).append("\n")
        sum += jointProb
      }
      stringBuilder.append(f"\n Sum of probs ${sum}")
      stringBuilder.result()
    }
    def calculateProbs() = {
      val pStrides = new ArrayBuffer[Int]
      pStrides.append(1)
      val pMap: mutable.HashMap[Int, Int] = new mutable.HashMap[Int, Int]
      val pSize: Int = nodes.foldLeft(1)((acc: Int, node: Node) => acc * node.nVars)
      jointProbabilities.append(1)
      val used: ArrayBuffer[Boolean] = ArrayBuffer.fill[Boolean](nodes.size)(false)

      val bfsQ: mutable.Queue[Int] = new mutable.Queue[Int]
      this.independent.foreach(x => bfsQ.enqueue(x))
      while (!bfsQ.isEmpty) {
        val nodeIndex = bfsQ.dequeue()
        if (!used(nodeIndex)) {
          val node = nodes(nodeIndex)
          if (node.ancestors.forall(i => used(i))) {
            used(nodeIndex) = true
            val oldJointSize = jointProbabilities.size
            val newJointProbs = new ArrayBuffer[Double]
            for (i <- 0 until node.nVars) {
              jointProbabilities.foreach(newJointProbs.append(_))
            }
            jointProbabilities = newJointProbs

            if (node.ancestors.isEmpty) {
              for (i <- 0 until jointProbabilities.size) {
                jointProbabilities(i) *= node.probabilities(i / oldJointSize)
              }
            } else {
              for (i <- 0 until jointProbabilities.size) {
                val (first, second) = translateOffset(pStrides, pMap, node.ancestors, i);
                jointProbabilities(i) *= node.conditionalProbabilities(second + (i / oldJointSize) * first);
              }

              for (v <- 0 until node.nVars) {
                var prob: Double = 0.0
                val offset = v * oldJointSize
                for (i <- 0 until oldJointSize) {
                  prob += jointProbabilities(offset + i)
                }
                node.probabilities(v) = prob
              }
            }
            pStrides.append(jointProbabilities.size)
            pMap.put(nodeIndex, path.size)
            path.append(nodeIndex)
            node.children.foreach(x => bfsQ.enqueue(x))
          } else {
            bfsQ.enqueue(nodeIndex)
          }
        }
      }
    }
  }


  var classDict = BiMap.empty
  var valueDict = new ArrayBuffer[BiMap]
  var name: String = "Unnamed"
  var graph: Graph = new Graph

  def process_declaration(xml: Elem) : Unit = {
    val nodes = (xml \ "declare" \ "node")
    var valueDict = new ArrayBuffer[BiMap]
    val classDict = BiMap.empty()
    for ((node, i) <- nodes.zipWithIndex)  {
      val text = (node \ "name").head.text
      val biMap = BiMap.empty()
      for ((value, j)  <- (node.head \ "values" \ "value").toArray.zipWithIndex) {
        biMap.put(value.text, j)
      }
      valueDict.append(biMap)
      classDict.put(text, i)
    }
    this.valueDict = valueDict
    this.classDict = classDict
  } // enf of process_declaration

  def process_definition(xml: Elem) : Unit = {
    graph.extend(valueDict)
    for (p <- (xml \ "define").head.nonEmptyChildren.filter(!_.label.startsWith("#"))) {
      val nodeIndex = classDict(p.label)// get Optional[Int]
      val node = graph.nodes(nodeIndex)
      val dependency = p \ "depends"
      node.probabilities = ArrayBuffer.fill[Double](node.nVars)(0.0)
      var condTableSize = node.nVars
      if (dependency.length == 1) {
        for (dependencyItem <- dependency.head \ "node") {
          val depIndex = classDict(dependencyItem.text)
          node.ancestors.append(depIndex)
          graph(depIndex).children.append(nodeIndex)
          condTableSize *= graph(depIndex).nVars
        }
        node.conditionalProbabilities = ArrayBuffer.fill[Double](condTableSize)(0.0)
        for (condition <- p \ "condition") {
          val (offset: Int, stride: Int) = node.ancestors.foldLeft((0, 1))((result: (Int, Int), dep) => {
            (result._1 + valueDict(dep).get(condition.attribute(classDict(dep)).get.text).get * result._2, result._2 * graph(dep).nVars)
          })
          for (variable <- 0 until node.nVars) {
            val field: String = valueDict(nodeIndex).get(variable).get
            node.conditionalProbabilities(offset + variable * stride) = (condition \ field).head.text.toDouble
          }
        }
      } else {
        graph.independent.append(nodeIndex)
        for (i <- 0 until node.nVars) {
          node.probabilities(i) = (p \ valueDict(nodeIndex).apply(i)).text.toDouble
        }
      }
    }

  } // end of processDefinition

  def mkStringProbs(): String = {
    graph.mkStringProbabilities()
  }

  def mkStringJointProbs() : String = {
    graph.mkStringJointProbabilities()
  }

  def calculateProbs() : Unit = {
    graph.calculateProbs()
  }
}

object BayesianNetwork {
  def fromXML(xml: Elem): BayesianNetwork = {
    val bayesianNetwork = new BayesianNetwork
    bayesianNetwork.process_declaration(xml)
    bayesianNetwork.process_definition(xml)
    bayesianNetwork.calculateProbs()
    bayesianNetwork
  }
}
