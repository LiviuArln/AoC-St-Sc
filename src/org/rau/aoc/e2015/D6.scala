package org.rau.aoc.e2015

import scala.util.matching.Regex

object D6 extends E2015 {
  import org.rau.aoc._

  val instructionsPattern: Regex = "(.*) (\\d*),(\\d*) through (\\d*),(\\d*)".r

  val instructions1 = Map[String, (Map[(Int, Int), Boolean], (Int, Int)) => Map[(Int, Int), Boolean]](
    "turn off" -> { (grid, light) => grid + (light -> false) },
    "toggle" -> { (grid, light) => val old = grid.getOrElse(light, false) ; grid + (light -> !old) },
    "turn on" -> { (grid, light) => grid + (light -> true) })

  val instructions2 = Map[String, (Map[(Int, Int), Int], (Int, Int)) => Map[(Int, Int), Int]](
    "turn off" -> { (grid, light) => 
      val old = grid.getOrElse(light, 0);  
      if(old == 0) grid
      else grid + (light -> (old - 1))
    },
    "toggle" -> { (grid, light) => 
      val old = grid.getOrElse(light, 0);  
      grid + (light -> (old + 2)) },
    "turn on" -> { (grid, light) =>  
      val old = grid.getOrElse(light, 0);  
      grid + (light -> (old + 1)) })

  def solution[T](instructions: Map[String, (Map[(Int, Int), T], (Int, Int)) => Map[(Int, Int), T]]) =
    puzzle(6).lines.toStream.map { line =>
      val mathces = instructionsPattern.findFirstMatchIn(line).get
      (mathces.group(1), mathces.group(2).toInt, mathces.group(3).toInt, mathces.group(4).toInt, mathces.group(5).toInt)
    }.flatMap {
      case (instruction, top, left, bottom, right) =>
        val i = instructions(instruction)
        (for (
          x <- top to bottom;
          y <- left to right
        ) yield x -> y -> i)
    }.foldLeft(Map[(Int, Int), T]()) { (grid, insts) =>
      insts._2(grid, (insts._1._1, insts._1._2))
    }

  def solution1 = solution(instructions1).filter(_._2).size
  def solution2 = solution(instructions2).map(_._2).sum

}