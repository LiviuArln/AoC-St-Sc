package org.rau.aoc.e2015

object D8 extends E2015 {
  //"\"\"\n\"abc\"\n\"aaa\\\"aaa\"\n\"\\x27\""
  def solution1 = {
    puzzle(8).lines.map { line =>
      line.size ->  line
          .replace("\\\\", "*")
          .replaceAll("\\\\[xX][0-9a-fA-F]{2}","*")
          .replace("\\\"", "*")
          .replaceAll("\"", "").size
    }.map { case (code, memory) => code - memory }.sum 
  }

  def solution2 =
    puzzle(8).lines.map { line =>
      line.size ->  line
          .replace("\"", "**")
          .replace("\\","**").size
    }.map { case (code, encoded) => encoded - code + 2 }.sum 
 
}