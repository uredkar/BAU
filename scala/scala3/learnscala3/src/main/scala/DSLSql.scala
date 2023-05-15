package com.komsonandmarch.dslsql

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import dotty.tools.dotc.core.Types.Type

enum MyType {
  case MyNumber, MyString
}

case class Column(name: String,mytype : MyType)

case class Schema(schemaDef : List[Column])

case class Table(name: String)(schema : Schema)

trait Query {

  def select(columns: String*): Query

  def from(table: String): Query
  def from(table: Table): Query
  def from(f:(Schema) => Table): Query

  def where(condition: String): Query
  def where(condition: (Query) => Boolean): Query 
  def orderBy(column: String, ascending: Boolean = true): Query

  def limit(limit: Int): Query

  def fetch(): Seq[Row]

  def join(table: String, on: String): Query 
}




case class Row(columns: Map[String, Any]) {

  def get(column: String): Any = columns(column)

}

class SqlDslImpl extends Query{
override def select(columns: String*): Query = {
    println(s"select $columns")
    this
  }
  override def fetch(): Seq[Row] = {
    ???
  }
  override def join(table: String, on: String): Query = {
    ???
  }
  override def from(table: String): Query = {
    println(s"from $table")
    this
  }

  override def from(table: Table): Query = {
    println(s"from $table")
    this
  }

  override def from(f:(Schema) => Table): Query = {
    println(s"from $f")
    this
  }
  override def where(condition: String): Query = {
    ???
  }

  override def where(condition: (Query) => Boolean): Query = {
    ???
  }

  override def limit(limit: Int): Query = {
    ???
  }

  override def orderBy(column: String, ascending: Boolean = true): Query = {
    ???
  }
}

object SqlDslImpl {

  def apply(): Query = new SqlDslImpl()

}





def dslsql = {
    println("dsl sql")
    val emp_schema = Schema(List(Column("Col1",MyType.MyNumber),Column("Col2",MyType.MyString)))

    val employee = Table("Employee")
    val employee_with_schema = Table("Employee")(emp_schema)

    
    val query = SqlDslImpl().
    select("*")
    .from("person")
    .from(employee)
    .from(employee_with_schema)
    .join("address", "person_id = id")
    .where("age > 18")
    .where(x => 10 > 20)

    query.fetch().foreach(row => println(row))

}

