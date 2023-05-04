package com.komsonandmarch.givens

trait Encoder[T] {
  def encode(data: T): String
}

trait DataSource[T] {
  def read(): Seq[T]
}


def processData[T](dataSource: DataSource[T])(using encoder: Encoder[T]): String = {
  val data = dataSource.read()
  val json = data.map(encoder.encode).mkString("[", ",", "]")
  // process the data and return result
  return json
}

case class Person(name: String, age: Int)

given Encoder[Map[String, String]] with {
  def encode(data: Map[String, String]): String = {
    // encode data as JSON string
    ""
  }
}

given Encoder[Seq[Person]] with {
  def encode(person: Seq[Person]): String = {
    // encode person as JSON string
    ""
  }
}


given Encoder[Person] with {
  def encode(person: Person): String = {
    // encode person as JSON string
    ""
  }
}

class CsvDataSource extends DataSource[Map[String, String]] {
  def read(): Seq[Map[String, String]] = {
    // read data from CSV file and return as Seq[Map[String, String]]
    Seq.empty
  }
}

class ExcelDataSource(filename: String) extends DataSource[Seq[Person]] {
  def read(): Seq[Seq[Person]] = {
    // read data from Excel sheet and convert to sequence of sequences of Person objects
    Seq(Seq(Person("Alice", 25), Person("Bob", 30), Person("Charlie", 35)),
      Seq(Person("Alice", 25), Person("Bob", 30), Person("Charlie", 35)))
  
  }
}

class DbDataSource extends DataSource[Person] {
  def read(): Seq[Person] = {
    // read data from database and return as Seq[Person]
    Seq.empty
  }
}

class PreferredPrompt(val preference: String)

object Greeter:
    def greet(name: String)(using prompt: PreferredPrompt) =
      println(s"Welcome, $name. The system is ready.")
      println(prompt.preference)

object JillsPrefs:
  given prompt: PreferredPrompt =
    PreferredPrompt("Your wish> ")

case class User(id: Int, name: String, email: String)

trait UserRepository {
  def findById(id: Int): Option[User]
  def findByEmail(email: String): Option[User]
}

class InMemoryUserRepository(users: List[User]) extends UserRepository {
  def findById(id: Int): Option[User] = users.find(_.id == id)
  def findByEmail(email: String): Option[User] = users.find(_.email == email)
}

given userRepository: UserRepository = new InMemoryUserRepository(List(
  User(1, "Alice", "alice@example.com"),
  User(2, "Bob", "bob@example.com"),
  User(3, "Charlie", "charlie@example.com")
))

def getUserById(id: Int)(using userRepository: UserRepository): Option[User] = {
  userRepository.findById(id)
}

def getUserByEmail(email: String)(using userRepository: UserRepository): Option[User] = {
  userRepository.findByEmail(email)
}


given orderingInt: Ordering[Int] with {
  def compare(x: Int, y: Int): Int =
    if (x < y) -1 else if (x > y) 1 else 0
}

given orderingString: Ordering[String] with {
  def compare(x: String, y: String): Int =
    x.compareTo(y)
}


    
def Chap13Given = 
    import JillsPrefs.prompt
    Greeter.greet("Jill")    
    val excelDataSource = ExcelDataSource("data.xlsx").read()
    val json = excelDataSource.flatten.map(summon[Encoder[Person]].encode).mkString("[", ",", "]")
    println(json)

    val data = Map("key1" -> "value1", "key2" -> "value2")
    val json1 = summon[Encoder[Map[String, String]]].encode(data)
    
    val csvData = processData(CsvDataSource())
    val excelData = processData(ExcelDataSource("C:/temp"))
    val dbData = processData(DbDataSource())
    
    
    // Use the Encoder to encode the data as JSON
    
    println(json)

    val alice = getUserById(1) match {
      case Some(user) => user.name
      case None => "User not found"
    }

    val user = getUserById(2).getOrElse(User(-1, "Unknown", "unknown@example.com"))
    println(user.name)
    
    val bob = getUserByEmail("bob@example.com")
    println(s"alice $alice bob $bob")

    val nums = List(5, 2, 8, 1, 0, 3)
    val sortedNums = nums.sorted(summon[Ordering[Int]])

    val words = List("orange", "banana", "apple", "pear")
    val sortedWords = words.sorted(summon[Ordering[String]])

    println(sortedNums) // prints List(0, 1, 2, 3, 5, 8)
    println(sortedWords) // prints List(apple, banana, orange, pear)

