package learnscala3

import java.math.BigInteger

def factorial_java(x: BigInteger): BigInteger =
    if x == BigInteger.ZERO then
        BigInteger.ONE
    else
        x.multiply(factorial_java(x.subtract(BigInteger.ONE)))

def factorial(x: BigInt): BigInt = 
    if x == 0 then 1 else x * factorial(x - 1)  

def map_ex =
  var capital = Map("US" -> "Washington", "France" -> "Paris")

  capital += ("Japan" -> "Tokyo")
  println(capital("France")) 




def javaScalaComp = 
    var v = MyClass(10,"ten")
    v.Print("Calling Java Class from Scala for fun")
    var names = List("Test","test")
    names.map(name => 
        v.NameHasUpperCase(name)
        val nameHasUC = name.exists(_.isUpper)
        println(f"\nName >>$name%s<< has upper case >>$nameHasUC%b<<")
    )

def funtional_predicate_checking = 
    val p: Int => Boolean = _ % 2 == 0
    val q: Int => Boolean = _ < 0
    val s = List(0,-1,-2,-3,-5, 1,2,3,4,5)
    val rs = s.exists(p) || s.exists(q) == s.exists(x => p(x) || q(x)) 
    println(f"\nfuntional_predicate_checking : Result $rs%b")

