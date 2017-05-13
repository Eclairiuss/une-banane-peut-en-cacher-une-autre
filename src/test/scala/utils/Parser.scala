package utils

import scala.collection.immutable.Map
import scala.util.parsing.combinator.RegexParsers

object CSVParserBoardTokenCheck extends RegexParsers{
  val file = scala.io.Source.fromURL(getClass.getResource("/BoardTokenCheckPosition.csv")).getLines()
  val seqTest = for {
    line <- file
    word <- line.split(",")
  } yield {
    word match {
      case "O" => true
      case  _  => false
    }
  }

  def transform(m:((Int,Int),Map[(Int,Int),Boolean]), b:Boolean):((Int,Int),Map[(Int,Int),Boolean]) = {
    val tmp = if (m._1._2 < file.length) {
      ((m._1._1,m._1._2+1),(m._1._1-1,m._1._2-1))
    } else {
      ((m._1._1+1,0),(m._1._1-1,m._1._2-1))
    }
    (tmp._1, (m._2 + (tmp._2 -> b)))
  }

  val initFold : ((Int,Int),Map[(Int,Int),Boolean]) = ((0,0),Map[(Int,Int),Boolean]())
  val values:Map[(Int,Int),Boolean] = (seqTest.foldLeft(initFold)(transform))._2
}

object CSVParserTrollCheckMovesPosition {

}