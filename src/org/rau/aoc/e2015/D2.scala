package org.rau.aoc.e2015

object D2 extends E2015 {
  import org.rau.aoc._

  case class Package(x: Int, y: Int, z: Int) {
    def sidesAreas = x * y :: y * z :: z * x :: Nil
    def papper = sidesAreas.sum * 2 + sidesAreas.min
    def perimeters = x + y :: y + z :: z + x :: Nil
    def ribon = perimeters.min * 2 + x * y * z
  }

  def solution1: Int =
    solution((_:Package).papper)

  def solution2: Int =
    solution((_:Package).ribon)

  def solution(f: Package => Int): Int = puzzle(2).lines.map { p =>
    val dimensions = p.split('x').map(_.toInt)
    f(Package(dimensions(0), dimensions(1), dimensions(2)))
  }.sum
}