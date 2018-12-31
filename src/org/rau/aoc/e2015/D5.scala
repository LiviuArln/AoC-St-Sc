package org.rau.aoc.e2015

object D5 extends E2015 {
  import org.rau.aoc._

  abstract class Rules {
    def all: List[String => Boolean]
    def satifies(aString: String) = all.filterNot(_(aString)).isEmpty
  }
  object Rules1 extends Rules {
    val vowels = "aeiou"
    val naughtyStrings = "ab" :: "cd" :: "pq" :: "xy" :: Nil

    def contains3vowels(aString: String) = aString.filter(vowels.contains(_)).size > 2
    def oneLetterTwiceInRow(aString: String) = aString.sliding(2).filter { s =>
      s(0) == s(1)
    }.size > 0
    def noNaughtyStrings(aString: String) = naughtyStrings.find(aString.contains).isEmpty

    def all = contains3vowels _ :: oneLetterTwiceInRow _ :: noNaughtyStrings _ :: Nil
  }
  object Rules2 extends Rules {
    val vowels = "aeiou"
    def twoLettersTwice(aString: String) = aString.sliding(2).toList.groupBy(identity)
      .map { case (twoLetters, twoLettersList) => twoLetters -> twoLettersList.size }
      .filter(_._2 > 1).exists {
        case (twoLetters, occurencies) =>
          occurencies > 2 || {
            val firstOccuence = aString.indexOf(twoLetters)
            val secondOccuence = aString.indexOf(twoLetters, firstOccuence + 1)
            firstOccuence + 1 != secondOccuence
          }
      }

    def letterRepeatsWith1Gap(aString: String) = aString.sliding(3).filter(s => s(0) == s(2)).size > 0
    def all = twoLettersTwice _ :: letterRepeatsWith1Gap _ :: Nil
  }

  def solution(rules: Rules) = puzzle(5).lines.filter(rules.satifies).size
  def solution1 = solution(Rules1)
  def solution2 = solution(Rules2)

}