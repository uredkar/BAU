



object `LearnGivens.worksheet` {
def args = `LearnGivens.worksheet_sc`.args$
/*<script>*///import scala.collection.list.order
trait A:
  given x: Int = 0

trait B:
  given x: Int = 1

object C extends B:
  println(summon[Int])

C

case class Person(name:String, age : Int)

given OrderingByName : Ordering[Person] with {
  def compare(a:Person,b:Person) : Int = a.name.compareTo(b.name)
}

given OrderingByAge : Ordering[Person] with {
  def compare(a:Person,b:Person) : Int = a.age.compareTo(b.age)
}

val people = List(Person("one",1),Person("two",2),Person("three",3))

// add given defination in companion object
// check imported given

people.sorted(using OrderingByAge)
people.sorted(using OrderingByName)

case class Num(value : Int)
given n: Num = Num(42)
def f(using x: Num): Int = x.value
print(f)
print(f(using Num(0)))

case class Name(text: String)
//given Name = Name("World")
def world: Name = Name("World")
//given world: Name = Name("World")
//lazy val world: Name = Name("World")
//val world: String = "World"
def greet(using name: Name) = s"Hello, ${name.text}!"
//println(greet)


trait Show[T] {
  def show(value: T): String
}

given Show[Int] with {
  def show(value: Int): String = s"The integer value is $value"
}

def printValue1[T](value: T)(using show: Show[T]): Unit = {
  val result = show.show(value)
  println(result)
}

def printValue2[T](value: T)(show: Show[T]): Unit = {
  val result = show.show(value)
  println(result)
}

def printValue3[Int](value: Int)(show: Show[Int]): Unit = {
  val result = show.show(value)
  println(result)
}

val number: Int = 42

printValue1(number)


val x = printValue2(number)

x(summon[Show[Int]])

val y = new Show[Int] { 
    def show(value:Int): String =
        s"new show instance $value"
}
printValue3(number)(y)
/*</script>*/ /*<generated>*/
/*</generated>*/
}

object `LearnGivens.worksheet_sc` {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }
  def main(args: Array[String]): Unit = {
    args$set(args)
    `LearnGivens.worksheet`.hashCode() // hasCode to clear scalac warning about pure expression in statement position
  }
}

