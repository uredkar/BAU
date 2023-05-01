package learnscala3
import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import scala.collection.mutable.Map

def array_init = 
    val greetStrings = new Array[String](3)
    greetStrings(0) = "hello"
    greetStrings(1) = ","
    greetStrings(2) = "world\n"
    greetStrings.foreach(println)
    greetStrings(1) = "|"
    greetStrings.foreach(println)
    
    println("Apply 1")
    for i <- 0 to 2 do
        print(greetStrings.apply(i)) // apply is how you get elements from a array
    
    println("Apply 2")
    for i <- 0.to(2) do
        print(greetStrings.apply(i)) // apply is how you get elements from a array

def list_use =
    val oneTwoThree = List(1,2,3,4)
    val oneTwo = List(1,2)
    val threeFour = List(3,4)
    val oneToFour= oneTwo ::: threeFour
    println(oneToFour)
    println(s"these list are equal ${oneToFour == oneTwoThree}") // s string are simple, f string are printf style added %s etc

def adding_list = 
    val twoThree = List(2, 3)
    val oneTwoThree1 = 1 :: twoThree  // add to begining of a list
    val oneTwoThree2 = 1 :: 2 :: 3 :: Nil // Nil signifies end of the list and Nil means empty List , right hand has to be list

def formatargs(args:List[String]) = args.mkString("\n")
