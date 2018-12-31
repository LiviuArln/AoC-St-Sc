package org.rau.aoc.e2015

import java.security.MessageDigest

object D4 extends E2015 {
  import org.rau.aoc._

  val MD5 = MessageDigest.getInstance("MD5")

  def solution(puzzle: String, prefix: String): Int = {
    println(">>"+puzzle+"<<")
    Stream.from(1).takeWhile { i =>
      val digest = MD5.digest((puzzle + i).getBytes).map("%02X" format _).mkString
      !digest.startsWith(prefix)
    }.last + 1
  }
  
  def solution1 = solution(puzzle(4).trim, "00000")
  def solution2 = solution(puzzle(4).trim, "000000")
  
}