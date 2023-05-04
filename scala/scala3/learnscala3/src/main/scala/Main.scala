package learnscala3
import com.komsonandmarch.givens as giv
import com.komsonandmarch.directorycrawler as dc
import com.komsonandmarch.patternmatching as pm

//To run using command line and outside sbt : scala3 .\target\scala-3.2.2\learnscala3_3-0.1.0-SNAPSHOT.jar
object MyApp {
@main
def Main(args: String*): Unit =
  println("---------------------------")
  args.foreach(arg => println(arg))
  println("---------------------------")
  args.foreach(println)
  println("Hello world!")
  println(msg)
  //dc.cli.docli(args.toArray) // _* is required as args are a variable number of arguments
  //chap1
  //chap2
  //pm.testModel
  giv.Chap13Given
}  

def msg = "I was compiled by Scala 3. :)"

def chap1 = 
  map_ex
  javaScalaComp
  fn_predicate_checking
  try {
    fn_noimplementation("this is test")
  }
  catch {
    case e: Throwable => println(f"exception caught $e")
  }

def chap2 = 
  array_init
  list_use
  formatargs(List("one","two","three"))