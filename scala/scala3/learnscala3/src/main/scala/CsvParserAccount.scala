package com.komsonandmarch.CsvParser
import scala.io.Source

def isRowBlank(columns: Array[Option[String]]): Boolean = {
  columns.forall {
    case Some(value) => value.trim.isEmpty
    case None => true
  }
}

trait HeaderFunction[A, B] extends PartialFunction[A, B] {
  def apply(x: A): B
  def isDefinedAt(x: A): Boolean
  
}

sealed trait ParseResult

case class Header(columns: Array[Option[String]]) extends ParseResult
case class Detail(value: Array[Option[String]]) extends ParseResult
case class BlankLine() extends ParseResult
case class Row(columns: Array[Option[String]]) extends ParseResult


val justRow: PartialFunction[Array[Option[String]], Row] = {
  case columns  => Row(columns)
  //case name: String => Header(name, Nil)
}

val blankLine: PartialFunction[Array[Option[String]], BlankLine] = {
  case columns if isRowBlank(columns)  => BlankLine()
  //case name: String => Header(name, Nil)
}

val header: PartialFunction[Array[Option[String]], Header] = {
  case columns: Array[Option[String]] => Header(columns)
  //case name: String => Header(name, Nil)
}

val details: PartialFunction[Array[Option[String]], Detail] = {
  case values: Array[Option[String]] => Detail(values)
  //case name: String => Header(name, Nil)
}

val sectionRow = blankLine orElse header orElse details orElse justRow

case class Document(sections: Option[Seq[Option[Section]]])
case class Section(desc:Option[String],header: Option[Row],details:Seq[Row])



object CsvParser {

  def parse[T](fileName: String)(f: Array[Option[String]] => T): List[T] = {
    val lines = Source.fromFile(fileName).getLines.toList
    lines.map(line => {
      val fields = line.split(",").map(s => Option(s.trim))
      f(fields)
    })
  }

  def parseRows(fields: Array[Option[String]]): ParseResult = {
        sectionRow(fields)
        //Row(fields) 

  }

}

def parse = 
    println("parse called")
    val rows = CsvParser.parse("C:/temp/accounts.csv")(CsvParser.parseRows)
    //val employees = CsvParser.parse("C:/temp/accounts.csv")(CsvParser.parseEmployee)
    rows.foreach(x => {
            x match {
                case r: Row => println(s"Parsed row: $r")
                case h: Header => {
                    print(s"\nLength ${h.columns.length}->")
                    h.columns.map(c => print(s"columns: $c |"))
                }
                case d: Detail => println(s"Parsed detail: $d")
                case b: BlankLine => println("Parsed blank line")
            }
    })
    //println(s"people and age $people.name,$people.age")
    

