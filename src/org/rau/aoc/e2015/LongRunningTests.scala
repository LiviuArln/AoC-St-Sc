package org.rau.aoc.e2015

import org.junit.Assert._
import org.junit.Test

class LongRunningTests {
  @Test
  def testD4_1 {
    assertEquals(346386, D4.solution1)
  }
  @Test
  def testD4_2 {
    assertEquals(9958218, D4.solution2)
  }
}