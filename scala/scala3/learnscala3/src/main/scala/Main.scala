package learnscala3




@main
def Main(args: String*): Unit =
  println("---------------------------")
  args.foreach(arg => println(arg))
  println("---------------------------")
  args.foreach(println)
  println("Hello world!")
  println(msg)
  
  //chap1
  chap2
  

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