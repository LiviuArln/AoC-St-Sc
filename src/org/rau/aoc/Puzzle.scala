package org.rau.aoc

import scala.util.Try
import scalaj.http.Http
import scala.io.Source
import java.io.File
import java.io.PrintWriter

object Puzzle {

  val session = Source.fromFile(".session").mkString
    
  def apply(year: Int, day: Int): String = {
    Try(Source.fromFile(new File("input", s"P$year-$day")).mkString).toOption.getOrElse(getInput(year,day))
  }
  
  def getInput(year: Int, day: Int): String = {
    val url = s"https://adventofcode.com/$year/day/$day/input"
    val input = Http(url).cookie("session", session).asString.body
    new File("input").mkdir
    new PrintWriter(new File("input", s"P$year-$day")) { 
      write(input)
      close 
    }
    input
  }
}