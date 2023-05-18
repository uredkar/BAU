import java.lang.ProcessBuilder.Redirect
def trimLeft(str: String) = str.dropWhile(_.isWhitespace)
def trimRight(str: String) = str.take(str.lastIndexWhere(!_.isWhitespace)+1)
def trimRight2(str: String) = trimLeft(str.reverse).reverse
def trim(str: String) = str.trim()

trimLeft(">hello>  <wo<  <rld < <")
trimRight("  <<s s s 1 xxx")
trimRight2("  <<s s s 1 xxx")
trim("  >red car<  ")

def testTrim() = {
  val str = "  \u001F  String with spaces \t  \n  \r "
  println("original  : |" + str + "|")
  println("trimLeft  : |" + trimLeft(str) + "|")
  println("trimRight : |" + trimRight(str) + "|")
  println("trimRight2: |" + trimRight2(str) + "|")
  println("trim      : |" + trim(str) + "|")
}

testTrim()
println(1+2*3)
if true then println("true")
def showPrice(x:Double, p : Double) : String = 
    val price = x * p
    if price > 0 then
        "this is too expensive"
    else
        price.toString()

val a = 1
def square(x:Int): Int = 
    val y = 
        val z = 10 * 3
        z
    println(s"$x,$y,$a")    
    y
square(10)    

val tenSq1 = 10 * 10
def tenSq2 = 10 * 10
tenSq2
tenSq2


case class Note():
  def tone: Int = ??? 
  def duration: Int = ???

sealed trait Shape
case class Rectangle(width: Int, height:Int) extends Shape
case class Circle(radius:Int) extends Shape

val rect = Rectangle(10,20)
val cir = Circle(10)

def m(s:Shape) = s match
    case c: Circle => println(s"radius ${c.radius}")
    case Rectangle(w,h) => println(s"rect $w $h")

    //case Circle(r) => println(s"radius $r")
    
val someShape1: Shape = rect
val someShape2: Shape = cir
m(rect)
m(cir)

enum PC(val v:String):
    case Red extends PC("X")
    case Blue extends PC("Y")

val col = PC.Red
val col2: PC = PC.Blue
PC.valueOf("Red")
PC.values(0)
col.v






