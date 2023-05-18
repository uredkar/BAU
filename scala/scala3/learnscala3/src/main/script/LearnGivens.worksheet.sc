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
