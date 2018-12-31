package org.rau.aoc.e2015

object D3 extends E2015 {
  import org.rau.aoc._

  type Position = (Int, Int)
  type Visited = Set[Position]

  abstract class Visitor(visited: Visited) {
    def next(move: Char): Visitor
    def next(from: Position, move: Char): Position = move match {
      case '>' => (from._1 + 1, from._2)
      case '<' => (from._1 - 1, from._2)
      case '^' => (from._1, from._2 + 1)
      case 'v' => (from._1, from._2 - 1)
    }
    def visitCount = visited.size
  }

  case class SantaVisitor(
    currentPosition: Position = (0, 0),
    visited:         Visited  = Set((0, 0)))
    extends Visitor(visited) {
    def next(move: Char) = {
      val newPosition = next(currentPosition, move)
      SantaVisitor(newPosition, visited + newPosition)
    }
  }

  case class SantaRobotVisitor(
    santaPosition: Position = (0, 0),
    robotPosition: Position = (0, 0),
    isSantasTurn:  Boolean  = true,
    visited:       Visited  = Set((0, 0)))
    extends Visitor(visited) {
    def next(move: Char) = {
      val (newSantaPosition, newRobotPosition) = if (isSantasTurn)
        (next(santaPosition, move), robotPosition)
      else
        (santaPosition, next(robotPosition, move))
      SantaRobotVisitor(
        newSantaPosition,
        newRobotPosition,
        !isSantasTurn,
        visited + newSantaPosition + newRobotPosition)
    }
  }

  def solution1: Int = solution(SantaVisitor())
  def solution2: Int = solution(SantaRobotVisitor())

  def solution(visitor: Visitor): Int = {
    puzzle(3).toStream.foldLeft(visitor) { (currentVisit, move) =>
      currentVisit.next(move)
    }.visitCount
  }
}