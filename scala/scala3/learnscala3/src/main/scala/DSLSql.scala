package com.komsonandmarch.dslsql

import com.komsonandmarch.macros._
import scala.collection.mutable
//import scala.quoted._
//import scala.compiletime.{ summonFrom, erasedValue, summonInline, constValue }
//import scala.deriving._

//import scala.tasty.inspector.*
//import scala.tasty._
//learning from https://github.dev/deusaquilus/derivation_examples/

import scala.quoted.* // imports Quotes, Expr


object PopulateSubMaps {
inline def populateMap[T](from: T, map: mutable.Map[String, Any]): Unit = 
  ${ populateMapImpl('from, 'map) }

inline def recurseGetMap[T](from: T): mutable.Map[String, Any] =
  val map = mutable.Map[String, Any]()
  populateMap(from, map)
  map

def populateMapImpl[T: Type](from: Expr[T], map: Expr[mutable.Map[String, Any]])(using qctx: Quotes): Expr[Unit] =
  import qctx.reflect._
  val cls = TypeRepr.of[T].classSymbol.get
  val selects = 
    cls.caseFields.map { f =>
      val field = from.asTerm.select(f)
      val value =
        // if you do just this: from.asTerm.select(f).asExpr inner maps will be empty because they don't know what type to infer on
        if (field.tpe <:< TypeRepr.of[Product])
          field.tpe.asType match
            case '[tpe] => 
              '{ recurseGetMap[tpe](${from.asTerm.select(f).asExprOf[tpe]}) }
        else
          from.asTerm.select(f).asExpr

      val key = Expr(f.name)
      '{ $map.put($key, $value) }
    }
  Expr.block(selects, '{ () })
}

def dslsql = {
  import Util._

  def useMacro() = {
    val p = Person("Joe", "Bloggs", 123)
    val map = mutable.Map[String, Any]()
    PopulateMap.populateMap(p, map)
    map.foreach(p => println(s"A ${p._1} -> ${p._2}"))
  }

  def useFunMacro() = {
    val p = Person("Joe", "Bloggs", 123)
    val map = PopulateMapFun.populateMap(p)
    map.foreach(p => println(s"B ${p._1} -> ${p._2}"))
    //println(map)
  }

  def funB() = {
    val p = Person("Joe", "Bloggs", 123)

    val map = mutable.Map[String, Any]()
    map.put("firstName",  p.firstName)
    map.put("lastName",  p.lastName)
    map.put("age",  p.age)
  }

  def funA() = {
    val p = Person("Joe", "Bloggs", 123)
    val fields = reflectCaseClassFields(p)
    val map = mutable.Map[String, Any]()
    for (field <- fields)
      map.put(field.getName,  field.invoke(p))
  }


  useMacro()
  useFunMacro()

  println("hello")
}








  
