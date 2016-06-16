package mca

import algorithm.bitset_impl.{AlgorithmBS, OrderNone, PivotTomita}
import utility.LoadFile

object TimeIt extends App {

  val filesToCheck = List("marknewman/marknewman-karate", "marknewman/marknewman-astro",
    "marknewman/marknewman-celegens", "marknewman/marknewman-netscience", "marknewman/marknewman-polblogs")

  for(fileName <- filesToCheck) {
    Console.printf("Running graph %s.\n", fileName)
    val javaGraph = LoadFile.loadGraphBS(fileName)
    val algorithmJava = new AlgorithmBS(javaGraph, new OrderNone(), new PivotTomita())
    algorithmJava.execute()
    Console.printf("Java:\t%d cliques\t\t\t%d ms\n", algorithmJava.getNumberOfCliques, algorithmJava.getTime)

    val graph = Graph(javaGraph)
    val start = System.nanoTime()
    val count = graph.maximalCliques.length
    val time = (System.nanoTime() - start) / 1000000
    Console.printf("Scala:\t%d cliques\t\t\t%d ms\n\n", count, time)
  }
}