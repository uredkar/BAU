



object `exhanding.worksheet` {
def args = `exhanding.worksheet_sc`.args$
/*<script>*/import scala.util.{Try, Success, Failure}

def parseAndDivide(dividendStr: String, divisorStr: String): Either[Throwable, Int] = {
  val dividendEither = Try(dividendStr.toInt).toEither
  val divisorEither = Try(divisorStr.toInt).toEither

  (dividendEither, divisorEither) match {
    case (Right(dividend), Right(divisor)) =>
      if (divisor != 0) {
        Right(dividend / divisor)
      } else {
        Left(new IllegalArgumentException("Cannot divide by zero"))
      }
    case (Left(dividendError), _) => Left(dividendError)
    case (_, Left(divisorError)) => Left(divisorError)
  }
}

// Usage:
val result1 = parseAndDivide("10", "2")      
val result2 = parseAndDivide("10", "0")      
val result3 = parseAndDivide("abc", "2")     


import scala.util.{Try, Success, Failure}

type Errors = Seq[String]
type Validated[A] = Either[Errors,A]

val validInt: Validated[Int] = Right(42)
val invalidInt: Validated[Int] = Left(Seq("Out of range ","Format not valid"))

validInt.map(n => n*2)

def validateBoth[A,B](validatedA: Validated[A],validatedB: Validated[A]): Validated[(A,B)] = 
    ???

def validateEach[A,B](as: Seq[A])(validate: A => Validated[B]): Validated[Seq[B]] = 
    /*as.foldLeft[Validated[Seq[B]]](Right(Vector.empty[B])) {
       (vxs,a)  =>
            val validatedB: Validated[B] = validate(a)
            validateBoth(vxs,validatedB).map((bs,b) => bs :+ b)
    }*/
    ???

// this reports either valid int or a list of errors
def processInput_2(input: List[String]):  List[scala.util.Try[Either[(String,String), Int]]] = {

    val validationResults = input.map { str =>
        Try {
            println(str)
            val value = str.toInt

            if (value >= 0 && value <= 100) {
                // Process the valid input
                //println(s"2Valid input: $value")
                Right(value)
            } else {
                Left("2Input out of range",str)
            }
        }.recoverWith { case ex: NumberFormatException =>
            // Handle NumberFormatException
            //println(s"2Invalid input: $str (${ex.getMessage})")
            Failure(ex)
            }.recoverWith { case ex: IllegalArgumentException =>
            // Handle IllegalArgumentException
            //println(s"2Invalid input: $str (${ex.getMessage})")
            Failure(ex)
            }
    }
    validationResults
}


def processInput(input: List[String]): Try[Unit] = {
  val validationResults = input.map { str =>
    Try {
      val value = str.toInt
      if (value >= 0 && value <= 100) {
        // Process the valid input
        //println(s"Valid input: $value")
      } else {
        throw new IllegalArgumentException("Input out of range")
      }
    }.recoverWith { case ex: NumberFormatException =>
      // Handle NumberFormatException
      //println(s"Invalid input: $str (${ex.getMessage})")
      Failure(ex)
    }.recoverWith { case ex: IllegalArgumentException =>
      // Handle IllegalArgumentException
      //println(s"Invalid input: $str (${ex.getMessage})")
      Failure(ex)
    }
  }

  Try(validationResults).map(_ => ())
}

// Usage:
val input = List("232","42", "abc", "-10", "150")
println(s"input $input")
val r1 = processInput(input)
println(s"single $r1")

val r2 = processInput_2(input)
println(r2)


//val tryInt: Try[Int] = Failure(":sadface:")
val eitherInt = Left(Throwable(":sadface:"))

val confused: Try[Throwable] = Try(Throwable("Am I a success ?"))


val eitherInt2: Either[String, Int] = Left(":sadface:")/*</script>*/ /*<generated>*/
/*</generated>*/
}

object `exhanding.worksheet_sc` {
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
    `exhanding.worksheet`.hashCode() // hasCode to clear scalac warning about pure expression in statement position
  }
}

