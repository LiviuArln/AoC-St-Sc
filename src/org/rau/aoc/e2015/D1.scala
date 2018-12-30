package org.rau.aoc.e2015

object D1 extends E2015 {

  def solution1: Int =
    solution1(puzzle(1))

  def solution1(puzzle: String): Int = {
    puzzle.foldLeft(0) { (s, c) =>
      c match {
        case '(' => s + 1;
        case ')' => s - 1;
      }
    }
  }

  def solution2: Int =
    solution2(puzzle(1))

  def solution2(puzzle: String): Int = {
    puzzle.zipWithIndex.toStream.scanLeft((0, -1)) { (s, x) =>
      x match {
        case ('(', index) => (s._1 + 1, index)
        case (')', index) => (s._1 - 1, index)
      }
    }.takeWhile(_._1 >= 0).last._2 + 2
  }
}