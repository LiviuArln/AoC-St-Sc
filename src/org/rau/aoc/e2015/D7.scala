package org.rau.aoc.e2015

import scala.util.Try
import scala.util.Either
import scala.util.Right

object D7 extends E2015 {
  import org.rau.aoc._

  abstract class Whire {
    def elevate(whireValues: Map[String, Int]): Whire
  }
  case class NamedWhire(name: String) extends Whire {
    def pair(value: ValuedWhire) = name -> value.value
    def elevate(whireValues: Map[String, Int]) = whireValues.get(name).map(ValuedWhire).getOrElse(this)
  }
  case class ValuedWhire(value: Int) extends Whire {
    def elevate(whireValues: Map[String, Int]) = this
  }

  abstract class Gate {
    def result(whireValues: Map[String, Int]): Option[(String, Int)]
    def elevate(whireValues: Map[String, Int]): Gate
    def solve(whireValues: Map[String, Int]) = {
      val elevated = this.elevate(whireValues)
      elevated -> elevated.result(whireValues);
    }
  }
  case class IDGate(input: Whire, output: NamedWhire) extends Gate {
    def result(whireValues: Map[String, Int]) = input match {
      case ValuedWhire(v) => Some(output.name -> v)
      case _              => None
    }
    def elevate(whireValues: Map[String, Int]): Gate = input.elevate(whireValues) match {
      case x if x != input => IDGate(x, output)
      case _               => this
    }
  }
  case class NOTGate(input: Whire, output: NamedWhire) extends Gate {
    def result(whireValues: Map[String, Int]) = input match {
      case ValuedWhire(v) => Some(output.name -> output(v))
      case _              => None
    }
    def output(v: Int) = (~v) & 0xFFFF
    def elevate(whireValues: Map[String, Int]) = input.elevate(whireValues) match {
      case x if x != input => NOTGate(x, output)
      case _               => this
    }
  }
  abstract class BinaryGate(input1: Whire, input2: Whire, output: NamedWhire) extends Gate {
    def result(whireValues: Map[String, Int]) = (input1, input2) match {
      case (ValuedWhire(v1), ValuedWhire(v2)) => Some(output.name -> (result(v1, v2) & 0xFFFF))
      case _                                  => None
    }
    def elevate(whireValues: Map[String, Int]) = (input1.elevate(whireValues), input2.elevate(whireValues)) match {
      case (i1, i2) if i1 != input1 || i2 != input2 => elevateWith(i1, i2, output)
      case _                                        => this
    }
    def elevateWith: (Whire, Whire, NamedWhire) => BinaryGate
    def result(v1: Int, v2: Int): Int
  }
  case class ANDGate(i1: Whire, i2: Whire, o: NamedWhire) extends BinaryGate(i1, i2, o) {
    def result(v1: Int, v2: Int) = v1 & v2
    def elevateWith = ANDGate(_, _, _)
  }
  case class ORGate(i1: Whire, i2: Whire, o: NamedWhire) extends BinaryGate(i1, i2, o) {
    def result(v1: Int, v2: Int) = v1 | v2
    def elevateWith = ORGate(_, _, _)
  }
  case class LSHIFTGate(i1: Whire, i2: Whire, o: NamedWhire) extends BinaryGate(i1, i2, o) {
    def result(v1: Int, v2: Int) = v1 << v2
    def elevateWith = LSHIFTGate(_, _, _)
  }
  case class RSHIFTGate(i1: Whire, i2: Whire, o: NamedWhire) extends BinaryGate(i1, i2, o) {
    def result(v1: Int, v2: Int) = v1 >> v2
    def elevateWith = RSHIFTGate(_, _, _)
  }

  def toStream(gates: List[Gate], solvedWhires: Map[String, Int]): Stream[(List[Gate], Map[String, Int])] =
    (gates, solvedWhires) match {
      case (Nil, _) => Stream.Empty
      case (gs, sws) => {
        val gatesWithResults = gs.map(_.solve(sws))
        val remainingGates = gatesWithResults.filter(_._2.isEmpty).map(_._1)
        val newSolvedWhires = gatesWithResults.filter(_._2.isDefined).map(_._2.get)
        (gs, sws) #:: toStream(remainingGates, sws ++ newSolvedWhires)
      }
    }

  def solution1 = {
    val gates = extracted()

    toStream(gates, Map()).last._2("lx")
  }
  def solution2 = {
    val gates = extracted()
    val originalA = toStream(gates, Map()).last._2("lx")
    toStream(gates.map { g =>
      g match {
        case IDGate(ValuedWhire(_), NamedWhire("b")) => IDGate(ValuedWhire(originalA), NamedWhire("b"))
        case _                                       => g
      }
    }, Map()).last._2("lx")
  }

  def extracted() = {
    val gates = puzzle(7).lines.toList.map { line =>
      val gate = line.split(" -> ")
      val output = NamedWhire(gate(1))
      val instr = gate(0).split(" ")

      if (instr.size == 1) {
        val i1 = Try(ValuedWhire(instr(0).toInt)).getOrElse(NamedWhire(instr(0)))
        IDGate(i1, output)
      } else if (instr.size == 2) {
        val i1 = Try(ValuedWhire(instr(1).toInt)).getOrElse(NamedWhire(instr(1)))
        NOTGate(i1, output)
      } else {
        val i1 = Try(ValuedWhire(instr(0).toInt)).getOrElse(NamedWhire(instr(0)))
        val i2 = Try(ValuedWhire(instr(2).toInt)).getOrElse(NamedWhire(instr(2)))
        instr(1) match {
          case "AND"    => ANDGate(i1, i2, output)
          case "OR"     => ORGate(i1, i2, output)
          case "LSHIFT" => LSHIFTGate(i1, i2, output)
          case "RSHIFT" => RSHIFTGate(i1, i2, output)
        }
      }
    }
    gates
  }
}