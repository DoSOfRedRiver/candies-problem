package com.gmail.dosofredriver.candies

import java.io.File

import scala.collection.immutable.Range.Inclusive
import scala.collection.mutable
import scala.io.Source

/**
  * Created by Александр on 04.12.2016.
  */
object Candies extends App {

  def xorSum(args: Int*): Int =
    if (args.isEmpty) 0
    else args.reduce(_ ^ _)

  def isSolutionExists(args: Int*): Boolean = {
    xorSum(args:_*) == 0
  }

  def genSeq(size: Int) = {
    def genSeqHelper(acc: Vector[Boolean], seed: Int): List[Vector[Boolean]] = {
      if (seed == 0) List(acc)
      else {
        val updatedAcc = acc.updated(seed, true)
        genSeqHelper(updatedAcc, seed - 1) ++ genSeqHelper(acc, seed - 1)
      }
    }

    genSeqHelper(Vector.fill(size)(false), size-1).dropRight(1)
  }

  def solve(args: Int*): Option[Int] = {
    if (!isSolutionExists(args:_*)) None
    else {
      val seq = genSeq(args.size)
      val res =
        for {
          permutation <- seq
          l = mutable.ListBuffer[Int]()
          r = mutable.ListBuffer[Int]()
        } yield {
          for ((flag, value) <- permutation zip args) {
            if (flag) l += value
            else r += value
          }

          if (xorSum(l: _*) == xorSum(r: _*))
            math.max(l.sum, r.sum)
          else 0
        }

      Some(res.max)
    }
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  //Main method start here
  val fileLines = Source.fromFile("in.txt").getLines()
  val numberOfTestCases = fileLines.next().toInt

  val numLists = new Array[Array[Int]](numberOfTestCases)
  for (l <- 0 until numberOfTestCases) {
    val numCount = fileLines.next.toInt
    numLists(l)= fileLines.next.split(' ').map(_.toInt).take(numCount)
  }

  numLists
    .map(arr => solve(arr:_*))


  printToFile(new File("out.txt")){ p =>
    numLists
      .map(arr => solve(arr:_*))
      .zipWithIndex
      .map{case(result, index) =>
        s"Case $index: ${result.flatMap(x => Some(x.toString)).getOrElse("NO")}"
      }
      .foreach(p.println)
  }
}
