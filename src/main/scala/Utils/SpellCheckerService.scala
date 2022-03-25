package Utils

import scala.annotation.tailrec
import scala.language.postfixOps

trait SpellCheckerService:
  /**
    * This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
    * we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
    */
  val dictionary: Map[String, String]

  /**
    * Calculate the Levenstein distance between two words.
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenstein distance between "s1" and "s2"
    */
  def stringDistance(s1: String, s2: String): Int

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number or a pseudonym, this function just returns it.
    * @param misspelledWord the misspelled word to correct
    * @return the closest normalized word from "misspelledWord"
    */
  def getClosestWordInDictionary(misspelledWord: String): String
end SpellCheckerService

class SpellCheckerImpl(val dictionary: Map[String, String]) extends SpellCheckerService:
  // TODO - Part 1 Step 2

  // Recursive version
  def stringDistance(s1: String, s2: String): Int =
    (s1, s2) match
      case (s1,s2) if (s1.length min s2.length) == 0 => s1.length max s2.length
      case (s1,s2) if s1.head == s2.head => stringDistance(s1.tail, s2.tail)
      case _ => 1 + (stringDistance(s1.tail, s2) min stringDistance(s1, s2.tail) min stringDistance(s1.tail, s2.tail))

//  // Tail recursive version
//  def stringDistance(s1: String, s2: String): Int = {
//
//    // Process the column of the levenshtein matrix. Each iteration we move to the next colunm.
//    @tailrec
//    def processRow(s1: String, s2: String, r:Array[Int], c:Int): Int = {
//      if s2.isEmpty then return r.last
//      val updateRow = processCol(s1,s2,r,0,c)
//      processRow(s1, s2.tail, updateRow, c+1)
//    }
//
//    // Process the row of the levenshtein matrix. Each iteration we process the next cell of the row.
//    @tailrec
//    def processCol(s1: String, s2: String, r:Array[Int], rIndex:Int, c:Int): Array[Int] = {
//      if s1.isEmpty then
//        r(rIndex) = c
//        return r
//      var current: Int =  c min r(rIndex) min r(rIndex+1)
//      if s1.head != s2.head then current += 1
//      r(rIndex) = c
//      processCol(s1.tail, s2, r, rIndex+1, current)
//    }
//
//    val row: Array[Int] = (0 to s1.length).toArray
//    processRow(s1,s2, row, 1)
//  }

  // TODO - Part 1 Step 2
  def getClosestWordInDictionary(misspelledWord: String): String =
    if misspelledWord.toDoubleOption.isDefined || misspelledWord.head == '_' then return misspelledWord

    val nearestWord = dictionary.foldLeft(("", Int.MaxValue))((acc, value) => {
      val dist = stringDistance(value._1, misspelledWord)
      if dist < acc._2 || (dist == acc._2 && value._1 < acc._1) then
        (value._1, dist)
      else
        acc
    })

    dictionary(nearestWord._1)

end SpellCheckerImpl
