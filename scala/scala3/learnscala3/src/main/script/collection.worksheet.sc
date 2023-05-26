import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration
import scala.util.{Success, Failure, Try}


// current working dir
val cwd = System.getProperty("user.dir")
println(s"cwd $cwd")


val files2: List[String] = List("file1.txt", "file2.txt", "file3.txt").map(
    "./src/main/script/" + _
)

val files1: List[String] = List("file1.txt", "file2.txt", "file3.txt")

var futures: List[Future[List[Try[Int]]]] = files2.map(processFile)

def processFile(file: String): Future[List[Try[Int]]] = Future {
  // Simulate file processing
  // Read the file and extract integers
  // For the sake of example, let's assume we are extracting integers from a file
  // and filtering out the odd values as invalid
  
  val lines: List[String] = scala.io.Source.fromFile(file).getLines().toList
  lines.map(line => Try(line.toInt))
}




val resultFuture: Future[(List[(String, String)], List[(String, String)])] = Future.sequence(futures).map { lists =>
  val (validLists, invalidLists) = lists.partition(_.forall(_.isSuccess))
  
  val validValues: List[(String, String)] = validLists.flatten.map { tryList =>
    val validNumbers = tryList match {
      case Success(number) if number % 2 == 0 => number.toString
      case _ => ""
    }
    (tryList.toString, validNumbers)
  }
  
  val invalidValues: List[(String, String)] = invalidLists.flatten.map { tryList =>
    val errorMessages = tryList match {
      case Failure(ex) => ex.getMessage
      case _ => ""
    } //.getOrElse("")
    (tryList.toString, errorMessages)
  }
  
  (validValues, invalidValues)
}

//val ret = Await.result(resultFuture, Duration.Inf)

val (valid, invalid) = Await.result(resultFuture, Duration.Inf)

println("Valid values:")
valid.foreach { case (input, validNumbers) =>
  println(s"Input: $input\nValid Numbers: $validNumbers\n")
}

println("Invalid values:")
invalid.foreach { case (input, errorMessage) =>
  println(s"Input: $input\nError Message: $errorMessage\n")
}
