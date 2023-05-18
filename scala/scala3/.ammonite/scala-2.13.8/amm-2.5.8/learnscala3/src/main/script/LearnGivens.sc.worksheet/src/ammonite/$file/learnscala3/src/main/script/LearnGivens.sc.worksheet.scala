
package ammonite
package $file.learnscala3.src.main.script
import _root_.ammonite.interp.api.InterpBridge.{
  value => interp
}
import _root_.ammonite.interp.api.InterpBridge.value.{
  exit,
  scalaVersion
}
import _root_.ammonite.interp.api.IvyConstructor.{
  ArtifactIdExt,
  GroupIdExt
}
import _root_.ammonite.compiler.CompilerExtensions.{
  CompilerInterpAPIExtensions,
  CompilerReplAPIExtensions
}
import _root_.ammonite.runtime.tools.{
  browse,
  grep,
  time,
  tail
}
import _root_.ammonite.compiler.tools.{
  desugar,
  source
}
import _root_.mainargs.{
  arg,
  main
}
import _root_.ammonite.repl.tools.Util.{
  PathRead
}
import _root_.ammonite.repl.ReplBridge.value.{
  codeColorsImplicit
}


object `LearnGivens.sc.worksheet`{
/*<script>*/trait Show[T] {
  def show(value: T): String
}

given Show[Int] with {
  def show(value: Int): String = s"The integer value is $value"
}

def printValue1[T](value: T)(using show: Show[T]): Unit = {
  val result = show.show(value)
  println(result)
}

def printValue2[T](value: T)(show: Show[T]): Unit = {
  val result = show.show(value)
  println(result)
}

def printValue3[Int](value: Int)(show: Show[Int]): Unit = {
  val result = show.show(value)
  println(result)
}

val number: Int = 42

/*<amm>*/val res_6 = /*</amm>*/printValue1(number)


val x = printValue2(number)

/*<amm>*/val res_8 = /*</amm>*/x(summon[Show[Int]])

val y = new Show[Int] { 
    def show(value:Int): String =
        s"new show instance $value"
}
/*<amm>*/val res_10 = /*</amm>*/printValue3(number)(y)
/*</script>*/ /*<generated>*/
def $main() = { _root_.scala.Iterator[String]() }
  override def toString = "LearnGivens$u002Esc$u002Eworksheet"
  /*</generated>*/
}
