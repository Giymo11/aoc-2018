
import scala.collection.immutable.{HashMap, HashSet}
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
    type TwoAndThreeList = (Seq[String], Seq[String])

    val emptyLists: TwoAndThreeList = (Seq[String](), Seq[String]())
    def collectTwoAndThreeLetterWords(lists: TwoAndThreeList, currentString: String): TwoAndThreeList = {

      val emptyMap = HashMap[Char, Int]().withDefaultValue(0)
      def updateCountEntry(map: Map[Char, Int], char: Char) = map + (char -> (map(char) + 1))
      val resultMap = currentString.foldLeft(emptyMap)(updateCountEntry)

      (if(resultMap.values.exists(_ == 2)) lists._1 ++ Seq(currentString) else lists._1,
        if(resultMap.values.exists(_ == 3)) lists._2 ++ Seq(currentString) else lists._2)
    }
    val (twos, threes) = inputStrings.foldLeft(emptyLists)(collectTwoAndThreeLetterWords)

    val checksum = twos.length * threes.length
    println(s"Tast1: $checksum")

    val searchspace = twos.combinations(2) ++ threes.combinations(2)
    for(combination <- searchspace) {
      val zipped = combination(0).zip(combination(1)).zipWithIndex

      type Index = Int
      def collectIndicesOfDifferences(acc: Set[Index], chars: ((Char, Char), Index)) = if(chars._1._1 != chars._1._2) acc + chars._2 else acc
      val indicesOfDifferences = zipped.foldLeft(Set[Index]())(collectIndicesOfDifferences)

      if(indicesOfDifferences.size == 1) {
        val allChars = combination.head
        val index = indicesOfDifferences.head
        val sameChars = allChars.take(index) + allChars.takeRight(allChars.length - index - 1)
        println(s"Task2: $sameChars")
        return
      }
    }
  }

}


object Main extends App {
  //Days.day1(Source.fromFile("input1.txt").getLines())
  //Days.day2(Source.fromFile("input2.txt").getLines())

}
