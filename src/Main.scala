import scala.collection.immutable.HashSet
import scala.io.Source

object Days {

  def day1(inputStrings: Iterator[String]) = {
    val input = inputStrings.map(_.toInt).toList
    println(s"Task1: ${input.sum}")

    def firstDoubleOccurance(set: Set[Int], iter: Iterator[Int], frequency: Int): Int = {
      val newFreq = frequency + iter.next()
      if(set(newFreq))
        newFreq
      else
        firstDoubleOccurance(set + newFreq, iter, newFreq)
    }
    println(s"Task2: ${firstDoubleOccurance(HashSet(0), Iterator.continually(input).flatten, 0)}")
  }

  def day2(inputStrings: Iterator[String]): Unit = {
    val input = inputStrings.toList

    def seperateByCharacter(string: String): Set[String] = string.groupBy(identity).values.toSet
    val grouped: Seq[Set[Int]] = input.map(seperateByCharacter(_).map(_.length))

    def filterWithLetterFrequency(freq: Int) = (input zip grouped).filter(_._2(freq)).map(_._1) // optimization: zip input with grouped outside of the function

    val candidates = for(freq <- 2 to 3) yield filterWithLetterFrequency(freq)
    val checksum = candidates.foldLeft(1)(_ * _.length)
    println(s"Tast1: $checksum")

    def equals(pair: (Char, Char)) = pair._1 == pair._2
    def countDifferences(left: String, right: String) = (left zip right).filterNot(equals).length
    def commonChars(left: String, right: String) = (left zip right).filter(equals).map(_._1).mkString("") // optimization: remember the index of the difference

    def findMatch(current: String, rest: Seq[String], diffs: Int): String =
      rest.find(countDifferences(_, current) == diffs) match {
        case Some(res) => commonChars(res, current)
        case _ => findMatch(rest.head, rest.tail, diffs)
      }
    val matches = candidates.map(list => findMatch(list.head, list.tail, diffs = 1))
    println(s"Task2: ${matches.head}")
  }
}

object Main extends App {
  //Days.day1(Source.fromFile("input1.txt").getLines())
  Days.day2(Source.fromFile("input2.txt").getLines())
}
