



object `week4.worksheet` {
def args = `week4.worksheet_sc`.args$
/*<script>*/val s = "8379" 
s.splitAt(1)
s.splitAt(2)
def split(digit: String) : Seq[Seq[String]] = 
    if digit.isEmpty then Seq(Nil)
    else
        for 
            splitPoint <- 1 to digit.length
            (first,remain) = digit.splitAt(splitPoint)
            digitSeq <- split(remain)
        yield 
            println(s"split point $splitPoint first $first, remain $remain")
            first +: digitSeq
val ss = split(s)                    
ss.map(elm => { 
    println(elm)
    //elm.map(println _)
})/*</script>*/ /*<generated>*/
/*</generated>*/
}

object `week4.worksheet_sc` {
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
    `week4.worksheet`.hashCode() // hasCode to clear scalac warning about pure expression in statement position
  }
}

