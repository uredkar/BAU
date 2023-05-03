
package ammonite
package $file.scala.scala3.learnscala3.src.main.scala.scriptworksheet
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


object hello{
/*<script>*//*<amm>*/val res_0 = /*</amm>*/6 * 7
val a = "world!"
/*<amm>*/val res_2 = /*</amm>*/"hello, " + a

def double(x: Int): Int =
  x * 2

/*<amm>*/val res_4 = /*</amm>*/double(3)
/*</script>*/ /*<generated>*/
def $main() = { _root_.scala.Iterator[String]() }
  override def toString = "hello"
  /*</generated>*/
}
