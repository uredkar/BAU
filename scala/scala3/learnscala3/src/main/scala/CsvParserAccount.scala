package com.komsonandmarch.CsvParser
import scala.io.Source

trait HeaderFunction[A, B] extends PartialFunction[A, B] {
  def apply(x: A): B
  def isDefinedAt(x: A): Boolean
  
}

case class Header(columns: List[Option[String]])
case class Detail(value: List[Option[String]])

val header: PartialFunction[List[Option[String]], Header] = {
  case columns: List[Option[String]] => Header(columns)
  //case name: String => Header(name, Nil)
}

val details: PartialFunction[List[Option[String]], Detail] = {
  case values: List[Option[String]] => Detail(values)
  //case name: String => Header(name, Nil)
}

val sectionRow = header orElse details

case class BlankLine()
case class Row(columns: Array[Option[String]])
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

  def parseRows(fields: Array[Option[String]]): Row = {
    if (fields.length > 2) { 
        Row(fields) 
    }
    else
        Row(fields)         
  }

}

def parse = 
    println("parse called")
    val rows = CsvParser.parse("C:/temp/accounts.csv")(CsvParser.parseRows)
    //val employees = CsvParser.parse("C:/temp/accounts.csv")(CsvParser.parseEmployee)
    rows.foreach(x => if (x.columns.length > 0) {
            println(x.columns(0))
    })
    //println(s"people and age $people.name,$people.age")
    

