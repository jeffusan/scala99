package search

import scala.annotation.tailrec
import scala.io.StdIn

object ConnectedCells {

  def main(args: Array[String]): Unit = {
    val in = read
//    println(analyse(in))
  }

  def read: Array[Int]= {

    val size = StdIn.readLine().toInt

    val input = Array.ofDim[Int](size)

    @tailrec
    def reread(i: Int): Array[Int] = {
      val s = Option(StdIn.readLine())
      if (s.isEmpty) input else {
        input(i) = s.get.toInt
        reread(i + 1)
      }
    }

    reread(0)
  }

  def analyse(grid: Array[Array[Int]]): (RegionCount, MaxSize) = {
    var maxSize = 0
    var regionCount = 0
    for(i <- grid.indices)
      for(j <- grid(0).indices)
        if(grid(i)(j) == 1){
          analyseCell(i, j)
        }

    def analyseCell(i: Int, j: Int): Unit = {
      regionCount = regionCount - 1
      val size = mark(i, j, regionCount)
      maxSize = Math.max(maxSize, size)
    }

    def mark(i: Int, j: Int, m: Int): Int = {
      if(i < 0 || j < 0 || i >= grid.length || j >= grid(0).length || grid(i)(j) != 1) 0
      else{
        grid(i)(j) = m
        1 + mark(i +1, j, m) +
          mark(i +1, j + 1,m) +
          mark(i, j+1,m) +
          mark(i-1, j+1,m) +
          mark(i-1, j,m) +
          mark(i-1,j-1,m) +
          mark(i, j-1, m) +
          mark(i+1, j-1,m)
      }
    }

    (RegionCount(-regionCount), MaxSize(maxSize))
  }

}


case class RegionCount(value: Int)
case class MaxSize(value: Int)