package learnscala3




@main
def Main(args: String*): Unit =
  println("Hello world!")
  println(msg)
  map_ex
  javaScalaComp
  fn_predicate_checking
  try {
    fn_noimplementation("this is test")
  }
  catch {
    case e: Throwable => println(f"exception caught $e")
  }
  //decl_map

def msg = "I was compiled by Scala 3. :)"

