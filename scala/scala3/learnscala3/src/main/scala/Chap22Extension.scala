package com.komsonandmarch.extension 

case class Security(symbol: String)

extension (s: Security) {
  def isOption: Boolean = s.symbol.endsWith("-O")
  def isFuture: Boolean = s.symbol.endsWith("-F")
}

object SecurityExtensions {
  extension [A](seq: Seq[A]) {
    def options: Seq[Security] = seq.collect { case s: Security if s.isOption => s }
    def futures: Seq[Security] = seq.collect { case s: Security if s.isFuture => s }
  }
}

val securities = Seq(
  Security("AAPL-O"),
  Security("GOOG-O"),
  Security("ES-F"),
  Security("NQ-F")
)

def Chap22 = 
    import SecurityExtensions.*

    println(securities.options) // prints: List(Security(AAPL-O), Security(GOOG-O))
    println(securities.futures) // prints: List(Security(ES-F), Security(NQ-F))