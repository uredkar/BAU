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
  def where(condition: (Table,Table) => Boolean): Query 
  def orderBy(column: String, ascending: Boolean = true): Query

  def limit(limit: Int): Query

  def fetch(): Seq[Row]

  def join(table: String, on: String): Query 

  def table(name: String): Table
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

  override def where(condition: (Table,Table) => Boolean): Query = {
    ???
  }

  override def limit(limit: Int): Query = {
    ???
  }

  override def orderBy(column: String, ascending: Boolean = true): Query = {
    ???
  }
  override def table(name: String): Table = ???
}

object SqlDslImpl {

  def apply(): Query = new SqlDslImpl()

}




class DSL[A] {
  private var filterConditions: List[A => Boolean] = List.empty

  def addFilterCondition(condition: A => Boolean): Unit =
    filterConditions = condition :: filterConditions

  def applyFilter(collection: List[A]): List[A] =
    collection.filter(item => filterConditions.forall(condition => condition(item)))
}

object DSL {
  def apply[A](block: DSL[A] => Unit): DSL[A] = {
    println("DSL Apply")
    val dsl = new DSL[A]
    block(dsl)
    dsl
  }
}

case class Product(name: String, price: Double)

val products = List(
  Product("Shirt", 29.99),
  Product("Jeans", 49.99),
  Product("Shoes", 79.99),
  Product("Hat", 14.99)
)

case class Person(name: String, age: Int)

val people = List(
  Person("John", 25),
  Person("Alice", 30),
  Person("Bob", 35),
  Person("Jane", 40)
)

val dsl_person = DSL[Person] { dsl =>
  println("dsl person creating...")
  dsl.addFilterCondition((person: Person) => person.age >= 30)
  dsl.addFilterCondition((person: Person) => person.name.startsWith("J"))
}

val dsl_product = DSL[Product] { dsl =>
  println("dsl product creating...")
  dsl.addFilterCondition(_.price <= 50)
  dsl.addFilterCondition(_.name.contains("Shirt"))
}

def dslsql = {
    println("in dslsql 1")
    val filteredPeople = dsl_person.applyFilter(people)
    println(filteredPeople)
    println("in dslsql 2")
    val filteredProducts = dsl_product.applyFilter(products)
    println(filteredProducts)

    println("dsl sql")
    val dept = Table("dept")
    val emp_schema = Schema(List(Column("Col1",MyType.MyNumber),Column("Col2",MyType.MyString)))

    val employee = Table("Employee")
    val employee_with_schema = Table("Employee")(emp_schema)
    /*
    
    val query = SqlDslImpl().
    select("*")
    .from("person")
    .from(employee)
    .from(employee_with_schema)
    .join("address", "person_id = id")
    .where("age > 18")
    .where((employee,dept) => if employee.name == "xxx" then true else false)
    //.where(x => x.employee.id  > 20 && 21 < 20)
    query.fetch().foreach(row => println(row))
    */
    

}

