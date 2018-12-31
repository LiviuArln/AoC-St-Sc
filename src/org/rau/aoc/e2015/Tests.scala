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
  @Test
  def testD3_1 {
    assertEquals(2081, D3.solution1)
  }
  @Test
  def testD3_2 {
    assertEquals(2341, D3.solution2)
  }
  @Test
  def testD5_1 {
    assertEquals(238, D5.solution1)
  }
  @Test
  def testD5_2 {
    assertTrue(D5.Rules2.satifies("qjhvhtzxzqqjkmpb"))
    assertTrue(D5.Rules2.satifies("xxyxx"))
    assertFalse(D5.Rules2.satifies("uurcxstgmygtbstg"))
    assertFalse(D5.Rules2.satifies("ieodomkazucvgmuy"))
    assertEquals(69, D5.solution2)
  }
}