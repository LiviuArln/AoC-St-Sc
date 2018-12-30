package org.rau.aoc.e2015

import org.junit.Assert._
import org.junit.Test

class Tests {
  @Test
  def testD1_1 {
    assertEquals(0, D1.solution1("(())"))
    assertEquals(0, D1.solution1("()()"))
    assertEquals(3, D1.solution1("((("))
    assertEquals(3, D1.solution1("(()(()("))
    assertEquals(3, D1.solution1("))((((("))
    assertEquals(-1, D1.solution1("())"))
    assertEquals(-1, D1.solution1("))("))
    assertEquals(-3, D1.solution1(")))"))
    assertEquals(-3, D1.solution1(")())())"))
    assertEquals(74, D1.solution1)
  }
  @Test
  def testD1_2 {
    assertEquals(1, D1.solution2(")"))
    assertEquals(5, D1.solution2("()())"))
    assertEquals(1795, D1.solution2)
  }
  @Test
  def testD2_1 {
    assertEquals(1598415, D2.solution1)
  }
  @Test
  def testD2_2 {
    assertEquals(3812909, D2.solution2)
  }
}