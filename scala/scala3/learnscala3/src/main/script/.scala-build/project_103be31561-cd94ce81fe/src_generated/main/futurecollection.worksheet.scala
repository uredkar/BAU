



object `futurecollection.worksheet` {
def args = `futurecollection.worksheet_sc`.args$
/*<script>*/import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext,Future, Await,Promise}
import scala.concurrent.duration.Duration
import scala.util.{Success, Failure, Try}
//given ec: ExecutionContext = scala.concurrent.ExecutionContext.global

// current working dir
def files_processsing() =
  val cwd = System.getProperty("user.dir")
  println(s"cwd $cwd")


  val files2: List[String] = List("file1.txt", "file2.txt", "file3.txt").map(
      "./src/main/script/" + _
  )

  val files1: List[String] = List("file1.txt", "file2.txt", "file3.txt")

  

  def processFile(file: String): Future[List[Try[Int]]] = Future {
    // Simulate file processing
    // Read the file and extract integers
    // For the sake of example, let's assume we are extracting integers from a file
    // and filtering out the odd values as invalid
    
    val lines: List[String] = scala.io.Source.fromFile(file).getLines().toList
    lines.map(line => Try(line.toInt))
  }



  var futures: List[Future[List[Try[Int]]]] = files2.map(processFile)
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




def performSomeOperation1(): Int = {
  // Simulating a time-consuming operation
  println("before 5 sec sleep")
  Thread.sleep(5000)
  println("after sleep")
  42
}

def main_foo1(): Unit = {
  given ec: ExecutionContext = ExecutionContext.global

  val futureResult: Future[Int] = Future {
    performSomeOperation1()
  }(ec)

  futureResult.onComplete {
    case Success(result) => println(s"1 Operation completed with result: $result")
    case Failure(ex) => println(s"1 Operation failed with exception: $ex")
  }(ec)

  // Block the main thread to allow the future to complete
  Thread.sleep(2000)
}

object Object23 {



  def performSomeOperation2(): Future[Int] = {
    // Simulating an asynchronous operation
    Future {
      // Simulating a time-consuming operation
      println("before 5 sec sleep")
      Thread.sleep(5000)
      println("after sleep")
      42
    }
  }

  def performAnotherOperation2(result: Int): Future[String] = {
    // Simulating an asynchronous operation
    Future {
      // Simulating a time-consuming operation
      println("before 1 sec sleep")
      Thread.sleep(1000)
      println("after sleep")
      
      s"Result: $result"
    }
  }

  def handleFailure(ex: Throwable): Future[String] = {
    // Handle the failure gracefully and return a default value
    Future.successful("Error occurred")
  }

  def main_foo2(): Unit = {
    given ec: ExecutionContext = ExecutionContext.global

    val futureResult: Future[String] = performSomeOperation2()
      .flatMap(performAnotherOperation2) // ec is implicit due to given
      .recoverWith(this.handleFailure) // ec is implicit due to given

    futureResult.onComplete {
      case Success(result) => println(s"2 Operation completed with result: $result")
      case Failure(ex) => println(s"2 Operation failed with exception: $ex")
    }(ec)

    // Block the main thread to allow the future to complete
    Thread.sleep(2000)
  }

  def main_foo3(): Unit = {
    val ec: ExecutionContext = ExecutionContext.global

    val futureResult: Future[String] = performSomeOperation2()
      .flatMap(performAnotherOperation2)(ec) // passing ec explicitly 
      .recoverWith(this.handleFailure)(ec) // passing ec explicity instead of using given

    futureResult.onComplete {
      case Success(result) => println(s"Operation completed with result: $result")
      case Failure(ex) => println(s"Operation failed with exception: $ex")
    }(ec)

    // Block the main thread to allow the future to complete
    Thread.sleep(2000)
}

}

def reader_writer() =
  // Shared resource
  val sharedList: scala.collection.mutable.ListBuffer[Int] = scala.collection.mutable.ListBuffer()
  println(s"shared $sharedList")
  // Read operation
  def readData(): Future[Seq[Int]] = Future {
    sharedList.toList
  }

  // Write operation
  def writeData(data: Int): Future[Unit] = Future {
    sharedList.synchronized {
      sharedList += data
    }
  }

  // Create multiple reader tasks
  val readerTasks: Seq[Future[Seq[Int]]] = (1 to 5).map { _ =>
    readData()
  }

  // Create multiple writer tasks
  val writerTasks: Seq[Future[Unit]] = (1 to 3).map { i =>
    writeData(i)
  }

  // Combine reader and writer tasks
  val allTasks: Seq[Future[_]] = readerTasks ++ writerTasks

  // Execute all tasks concurrently
  //implicit val ec: ExecutionContext = ExecutionContext.global
  val result: Future[Seq[_]] = Future.sequence(allTasks)

  // Await the completion of all tasks (for demonstration purposes only)
  val output: Seq[_] = Await.result(result, scala.concurrent.duration.Duration.Inf)



  // Print the final shared list
  println(s"Shared List: $output")







//files_processsing()
main_foo1()
Object23.main_foo2()
Object23.main_foo3()
reader_writer()


import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Success, Failure}


def performSomeOperation4(): Future[Int] = {
  // Simulating an asynchronous operation
  Future {
    // Simulating a time-consuming operation
    Thread.sleep(4000)
    println("420 done")
    420
  }
}
def performAnotherOperation4(result: Int): Future[String] = {
  // Simulating an asynchronous operation
  Future {
    // Simulating a time-consuming operation
    Thread.sleep(1000)
    println("Another4 done")
    throw new RuntimeException("Processing failed!")
    s"Result: $result"
  }
}

def handleFailure4(ex: Throwable): Future[String] = {
  // Handle the failure gracefully and return a default value
  Future.successful("Error occurred")
}

def dowork4(): Unit = {
  given ec: ExecutionContext = ExecutionContext.global

  val futureResult: Future[String] = performSomeOperation4()
    .flatMap(performAnotherOperation4)
    .recoverWith(handleFailure4)

  futureResult.onComplete {
    case Success(result) => println(s"4 Operation completed with result: $result")
    case Failure(ex) => println(s"4 Operation failed with exception: $ex")
  }(ec)
  println("do work4")
  // Block the main thread to allow the future to complete
  Thread.sleep(2000)
}

dowork4()

 Thread.sleep(10000)/*</script>*/ /*<generated>*/
/*</generated>*/
}

object `futurecollection.worksheet_sc` {
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
    `futurecollection.worksheet`.hashCode() // hasCode to clear scalac warning about pure expression in statement position
  }
}

