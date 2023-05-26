val result: Either[String, Int] = Right(42)

val matchedValue = result match {
  //case nofail @ Right(_) => nofail
  case Right(x) => Right(x)
  case _ => Left("Failure")
}


val matchedValue2 = result match {
  case nofail @ Right(_) => nofail // pattern binder equivalent to Right(x) see above
  case _ => Left("Failure")
}

println(matchedValue)
println(matchedValue2)
