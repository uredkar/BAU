package com.komsonandmarch.macros

import dotty.tools.dotc.ast.Trees._
//import scala.tasty.inspector._
import scala.quoted._
import scala.collection.mutable;
//import scala.quoted._
import scala.compiletime.{ summonFrom, erasedValue, summonInline, constValue }
import scala.deriving._

import scala.collection.mutable;
import scala.quoted._
import scala.compiletime.{ summonFrom, erasedValue, summonInline, constValue }
import scala.deriving._

object Util {
  case class MediumObject(field1: Int, field2: Int, field3: Int, field4: Int, field5: Int, field6: Int, field7: Int, field8: Int, field9: Int, field10: Int)
  object MediumObject {
    def random: MediumObject = MediumObject(rand(), rand(), rand(), rand(), rand(), rand(), rand(), rand(), rand(), rand())

  }
  def rand() = scala.util.Random.nextInt()

  case class Person(firstName: String, lastName: String, age: Int)

  def reflectCaseClassFields[T <: Product](p: T) = {
    val fieldNames = p.getClass.getDeclaredFields.map(_.getName)
    p.getClass.getDeclaredMethods.filter(m => fieldNames.contains(m.getName)).toList
  }

  val p = Person("Joe", "Bloggs", 123)
  val fieldsPre = reflectCaseClassFields(p)
}

object PopulateMapFun {
inline def populateMap[T](from: T): scala.collection.MapView[String, Any] =
  val myMap = mutable.Map[String, Any]()
  populateMapBlock(from, myMap)
  myMap.view

inline def populateMapBlock[T](from: T, map: mutable.Map[String, Any]): Unit = 
  ${ populateMapImpl('from, 'map) }

def populateMapImpl[T: Type](
  from: Expr[T], 
  map: Expr[mutable.Map[String, Any]]
)(using qctx: Quotes): Expr[Unit] =
  import qctx.reflect._
  val cls = TypeRepr.of[T].classSymbol.get
  val selects = 
    cls.caseFields.map { f =>
      val value = from.asTerm.select(f).asExpr
      val key = Expr(f.name)
      '{ $map.put($key, $value) }
    }
  Expr.block(selects, '{ () })
}

object PopulateMap {
  inline def populateMap[T](from: T, map: mutable.Map[String, Any]): Unit = 
    ${ populateMapImpl('from, 'map) }

  def populateMapImpl[T: Type](from: Expr[T], map: Expr[mutable.Map[String, Any]])(using qctx: Quotes): Expr[Unit] = {
    import qctx.reflect._
    val cls = TypeRepr.of[T].classSymbol.get
    val selects = 
      cls.caseFields.map { f =>
        val value = from.asTerm.select(f).asExpr
        val key = Expr(f.name)
        '{ $map.put($key, $value) }
      }
    Expr.block(selects, '{ () })
  }
}
