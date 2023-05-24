
trait Show[A] {
  def show(value: A): String
}

given Show[Int] with {
  def show(value: Int): String = s"The integer value is $value"
}

given [A](using showA: Show[A]): Show[Option[A]] with {
  def show(value: Option[A]): String = value match {
    case Some(a) => s"Some(${showA.show(a)})"
    case None    => "None"
  }
}

given [A](using showA: Show[A]): Show[List[A]] with {
  def show(value: List[A]): String = value.map(showA.show).mkString(", ")
}

def printUsingShow[A](value: A)(using showA: Show[A]): Unit =
  println(showA.show(value))

val num: Int = 42
val opt: Option[Int] = Some(10)
val list: List[String] = List("Alice", "Bob", "Charlie")

printUsingShow(num)   // Output: 42
printUsingShow(opt)   // Output: Some(10)
printUsingShow(list)  // Output: Alice, Bob, Charlie

trait LowPriorityInstances:
  given intOrdering: Ordering[Int] = Ordering.Int

object TLow extends LowPriorityInstances:

  given intReverseOrdering: Ordering[Int] = Ordering.Int.reverse

  def test: Unit =
    println(List(1, 2,4,5, 3).min) // returns max if reverse order is used

end TLow

TLow.test

List(1,2,3).min(using Ordering.Int.reverse)

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
