package mca

import graph.GraphBitSet

import scala.collection.immutable.Seq

class Graph private (val adjacencyMatrix: Seq[BitSet]) extends BronKerbosch with TomitaPivotSelection {

  def size = adjacencyMatrix.size

  def neighbours(v: Int) = adjacencyMatrix(v)

  override def selectPivot(bk: BronKerbosch, P: BitSet, X: BitSet): Int = super[TomitaPivotSelection].selectPivot(bk, P, X)
}

object Graph {
  def apply(graphBitSet: GraphBitSet) = {
    val matrix = new Array[BitSet](graphBitSet.size)
    for (v <- 0 to matrix.size - 1)
      matrix(v) = BitSet(graphBitSet.neighbours(v))
    new Graph(matrix.to[Seq])
  }

  def printGraph(graph: Graph) = {
    graph.adjacencyMatrix.foreach(println _)
  }
}
